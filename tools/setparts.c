#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <linux/fs.h>

#ifndef SEEK_DATA
#define SEEK_DATA 3
#endif

#ifndef SEEK_HOLE
#define SEEK_HOLE 4
#endif

struct partinfo {
        char *dstname;
        const char *srcname;
        off_t dstsz;
        off_t srcsz;
        int dstfd;
        int srcfd;
};

static void
usage(void)
{
        dprintf(2, "usage: setparts <disk> <parts>...\n");
        _exit(1);
}

static void
copy_range(int dstfd, int srcfd, off_t off, off_t end, char *buf, size_t bufsz)
{
        ssize_t rc;
        off_t width;
        assert(end >= off);

        width = end - off;
        while (off < end) {
                rc = pread(srcfd, buf, width < bufsz ? width : bufsz, off);
                if (rc < 0)
                        err(1, "pread(%d, ...)", srcfd);
                if (rc == 0)
                        errx(1, "unexpected EOF");
                rc = pwrite(dstfd, buf, rc, off);
                if (rc < 0)
                        err(1, "pwrite(%d, ...)", dstfd);
                off += rc;
                width = end - off;
        }
}

static void
fdcopy(int dstfd, int srcfd, off_t bytes)
{
        off_t srcoff, srcend;
        char buf[4096 * 16];

        srcoff = 0;
        while (srcoff < bytes) {
                srcoff = lseek(srcfd, srcoff, SEEK_DATA);
                if (srcoff < 0)
                        err(1, "lseek(%d, %llu, SEEK_DATA)", srcfd, (unsigned long long)srcoff);
                if (srcoff == bytes)
                        break;
                srcend = lseek(srcfd, srcoff, SEEK_HOLE);
                if (srcend < 0)
                        err(1, "lseek(%d, %llu, SEEK_HOLE)", srcfd, (unsigned long long)srcoff);
                copy_range(dstfd, srcfd, srcoff, srcend, buf, sizeof(buf));
                srcoff = srcend;
        }
        if (fsync(dstfd) < 0)
                err(1, "fsync(%d)", dstfd);
}

int
main(int argc, const char **argv)
{
        const char *disk;
        struct partinfo *parts;
        const char **partnames;
        struct stat st;
        int nparts;

        if (argc < 3)
                usage();

        disk = argv[1];
        partnames = argv+2;
        nparts = argc-2;

        /* TODO: allow custom partition naming?
         * some mtd and emmc schemes do not use
         * the typical <dev>p<part> formatting... */
        parts = calloc(sizeof(struct partinfo), nparts);
        assert(parts);
        for (int i=0; i<nparts; i++) {
                asprintf(&parts[i].dstname, "%sp%d", disk, i+1);
                parts[i].srcname = partnames[i];
        }

        for (int i=0; i<nparts; i++) {
                if (parts[i].dstname == NULL)
                        errx(1, "out of memory");
                if (strcmp(parts[i].srcname, "-") == 0)
                        continue;
                if ((parts[i].dstfd = open(parts[i].dstname, O_WRONLY)) < 0)
                        err(1, "open %s", parts[i].dstname);
                if ((parts[i].srcfd = open(parts[i].srcname, O_RDONLY)) < 0)
                        err(1, "open %s", parts[i].srcname);
                if (fstat(parts[i].dstfd, &st) < 0)
                        err(1, "stat %s", parts[i].dstname);
                parts[i].dstsz = st.st_size;
                if (parts[i].dstsz == 0)
                        ioctl(parts[i].dstfd, BLKGETSIZE64, &parts[i].dstsz);

                if (fstat(parts[i].srcfd, &st) < 0)
                        err(1, "stat %s", parts[i].srcname);
                parts[i].srcsz = st.st_size;

                if (parts[i].srcsz > parts[i].dstsz)
                        errx(1, "part %d: file %s won't fit in %llu", i+1, parts[i].srcname, (unsigned long long)parts[i].dstsz);
        }

        /* now that we've done some cursory validation,
         * do the actual copying */
        for (int i=0; i<nparts; i++)
                if (parts[i].srcsz) fdcopy(parts[i].dstfd, parts[i].srcfd, parts[i].srcsz);
        return 0;
}
