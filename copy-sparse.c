#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <stdbool.h>

/* musl unistd.h does not define SEEK_DATA ... */
#ifndef SEEK_DATA
#define SEEK_DATA 3
#endif

#ifndef SEEK_HOLE
#define SEEK_HOLE 4
#endif

#define really(expr) while ((expr)<0 && errno == EINTR)

/* copy_file_sparse() either renames 'from' to
 * 'to,' or copies a sparse file and takes care
 * to preserve holes in the file */
static int
copy_file_sparse(const char *from, const char *to, bool canrename)
{
    /* a guess at filesystem block granularity */
    int fdfrom, fdto;
    struct stat stbuf;
    int err = 0;
    off_t off, off2, end, sz;
    ssize_t ln;

    /* easy case: just rename */
    if (canrename) {
	really(err = rename(from, to));
	if (!err) return 0;
	err = 0;
    }

    really(fdfrom = open(from, O_RDONLY|O_CLOEXEC));
    if (fdfrom == -1)
	return -errno;

    really(fdto = open(to, O_CREAT|O_CLOEXEC|O_WRONLY|O_EXCL, 0644));
    if (fdto == -1) {
	err = errno;
	close(fdfrom);
	return -err;
    }

    really(err = fstat(fdfrom, &stbuf));
    if (err) { err = -errno; goto end; }

    sz = stbuf.st_size;
    really(err = ftruncate(fdto, sz));
    if (err) { err = -errno; goto end; }

    off = 0;
    /* for each section of actual data in the file,
     * use copy_file_range to move the data into the new file */
    while (off < sz) {
	really(off = lseek(fdfrom, off, SEEK_DATA));
	if (off < 0) {
	    if (errno == ENXIO) {
		/* see the man pages for lseek(2):
		 * ENXIO can occur if we are inside a hole
		 * at the end of the file (in which case we are done) */
		off = sz;
	    } else {
		err = -errno;
		goto end;
	    }
	}
	if (off == sz)
	    break;
	really(end = lseek(fdfrom, off, SEEK_HOLE));
	if (end < 0) {
	    err = -errno;
	    goto end;
	}
	off2 = off;
	while (off < end) {
	    really(ln = copy_file_range(fdfrom, &off, fdto, &off2, end-off, 0));
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
