#include <assert.h>
#include <limits.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "blake2.h"

/* convert a 32-byte hash into a 44-byte printable string
 * by base64(url)-encoding it */
static inline void
hash_to_base64(unsigned char *restrict dst, const unsigned char *restrict src)
{
    const unsigned char *alph = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    unsigned v;
    int i, di;

    di = 0;
    for (i=0; i<30; i+=3) {
        v = ((unsigned)src[i] << 16) | ((unsigned)src[i+1] << 8) | (unsigned)src[i+2];
        dst[di+0] = alph[(v>>18)&0x3f];
        dst[di+1] = alph[(v>>12)&0x3f];
        dst[di+2] = alph[(v>>6)&0x3f];
        dst[di+3] = alph[v&0x3f];
        di += 4;
    }
    /* tail: 2 bytes */
    v = ((unsigned)src[30] << 16) | ((unsigned)src[31] << 8);
    dst[40] = alph[(v>>18)&0x3f];
    dst[41] = alph[(v>>12)&0x3f];
    dst[42] = alph[(v>>6)&0x3f];
    dst[43] = '=';
}

/* fast-path for file hashing */
static inline int
fast_hash_file(unsigned char *restrict dst, const char *fname)
{
    unsigned char buf[32];
    unsigned char *mem;
    struct stat stbuf;
    blake2b_state S;
    int err, fd;

    fd = open(fname, O_RDONLY|O_CLOEXEC);
    if (fd < 0)
        return errno;

    if (fstat(fd, &stbuf) < 0)
        goto fail;

    /* use slow fallback */
    if (stbuf.st_rdev || stbuf.st_size > SSIZE_MAX) {
        errno = EINVAL;
        goto fail;
    }

    mem = mmap(NULL, (size_t)stbuf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (mem == MAP_FAILED)
        goto fail;

    while (close(fd) < 0 && errno == EINTR) ;
    err = blake2b(buf, 32, mem, stbuf.st_size, NULL, 0);
    assert(err == 0);
    hash_to_base64(dst, buf);

    while (munmap(mem, stbuf.st_size) < 0 && errno == EINTR) ;
    return 0;
fail:
    err = errno;
    while (close(fd) < 0 && errno == EINTR) ;
    return err;
}
