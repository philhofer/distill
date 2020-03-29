#define _GNU_SOURCE 1
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>

/* musl unistd.h does not define SEEK_DATA ... */
#ifndef SEEK_DATA
#define SEEK_DATA 3
#endif

#ifndef SEEK_HOLE
#define SEEK_HOLE 4
#endif

/* copy_file_sparse() either hardlinks 'from' to
 * 'to,' or copies a sparse file and takes care
 * to preserve holes in the file */
static int
copy_file_sparse(const char *from, const char *to)
{
        /* a guess at filesystem block granularity */
        int fdfrom, fdto;
        struct stat stbuf;
        int err = 0;
        off_t off, off2, end, sz;
        ssize_t ln;

        /* easy case: just create a hard link */
        if (link(from, to) == 0)
                return 0;
        fdfrom = open(from, O_RDONLY|O_CLOEXEC);
        if (fdfrom == -1)
                return -errno;
        fdto = open(to, O_CREAT|O_CLOEXEC|O_WRONLY|O_EXCL);
        if (fdto == -1) {
                close(fdfrom);
                return -errno;
        }

        if (fstat(fdfrom, &stbuf) < 0) {
                err = -errno;
                goto end;
        }
        sz = stbuf.st_size;
        if (ftruncate(fdto, sz) < 0) {
                err = -errno;
                goto end;
        }
        off = 0;
        /* for each section of actual data in the file,
         * use copy_file_range to move the data into the new file */
        while (off < sz) {
                off = lseek(fdfrom, off, SEEK_DATA);
                if (off < 0) {
                        err = -errno;
                        goto end;
                }
                if (off == sz)
                        break;
                end = lseek(fdfrom, off, SEEK_HOLE);
                if (end < 0) {
                        err = -errno;
                        goto end;
                }
                off2 = off;
                while (off < end) {
                        ln = copy_file_range(fdfrom, &off, fdto, &off2, end-off, 0);
                        if (ln < 0) {
                                err = -errno;
                                goto end;
                        }
                }
                assert(off == end);
        }
        assert(off == sz);
end:
        close(fdfrom);
        close(fdto);
        return err;
}
