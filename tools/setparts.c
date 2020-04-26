#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <err.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <linux/fs.h>

#ifndef SEEK_DATA
#define SEEK_DATA 3
#endif

#ifndef SEEK_HOLE
#define SEEK_HOLE 4
#endif

struct partinfo;

struct partinfo {
    struct partinfo *next;
    int   dstfd;
    int   srcfd;
    off_t copy;
};

const char *usagestr = \
    "usage: setparts { part contents ... } prog ...\n" \
    "    setparts copies data from 'contents' into 'part'\n" \
    "    for each of the named part + contents pairs, taking care\n" \
    "    not to copy holes in 'contents,' and finally exec-ing into 'prog'\n" \
    "    for example:\n" \
    "    # execlineb -c 'setparts { /dev/sda1 /boot/efi.img /dev/sda2 /path/to/rootfs.img } echo done!'\n";

static void
usage(void)
{
    dprintf(2, usagestr);
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

off_t
getsize(int fd)
{
    struct stat st;
    off_t out = 0;

    if (fstat(fd, &st) == 0)
	out = st.st_size;
    if (!out && st.st_rdev)
	out = ioctl(fd, BLKGETSIZE64, &out) == 0 ? out : 0;
    return out;
}

int
main(int argc, char * const* argv)
{
    struct partinfo *head, *tail, *part;
    const char *partname;
    const char *contents;
    off_t srcsz, dstsz;
    int srcfd, dstfd;

    argc--; argv++;
    if (argc <= 0)
	usage();

    head = tail = NULL;
    while (argc && strcmp(argv[0], "")) {
	if (argc < 2)
	    usage();
	partname = *argv++;
	contents = *argv++;
	argc -= 2;
	if (*partname++ != ' ' || *contents++ != ' ')
	    usage();
	if ((dstfd = open(partname, O_WRONLY|O_CLOEXEC)) < 0)
	    err(1, "open(%s)", partname);
	if ((srcfd = open(contents, O_RDONLY|O_CLOEXEC)) < 0)
	    err(1, "open(%s)", contents);
	dstsz = getsize(dstfd);
	srcsz = getsize(srcfd);

	if (dstsz < srcsz || !dstsz)
	    err(1, "%s doesn't fit in %s", contents, partname);
	if (srcsz) {
	    part = calloc(1, sizeof(struct partinfo));
	    part->srcfd = srcfd;
	    part->dstfd = dstfd;
	    part->copy = srcsz;
	    if (tail)
		tail->next = part;
	    else
		head = part;
	    tail = part;
	} else {
	    dprintf(3, "setparts: warning: %s is zero-sized (skipping)\n", contents);
	    close(dstfd);
	    close(srcfd);
	}
    }
    if (!argc-- || strcmp(*argv++, ""))
	usage();

    /* now that we've parsed the arguments, do the work
     * (just leak the partinfo mem; we're about to exit) */
    while (head) {
	fdcopy(head->dstfd, head->srcfd, head->copy);
	close(head->dstfd);
	close(head->srcfd);
	head = head->next;
    }

    if (!argc)
	return 0;
    execvp(argv[0], argv);
    err(1, "execve");
    return 1;
}
