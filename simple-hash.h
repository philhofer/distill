#include <assert.h>
#include <stdio.h>
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

static inline void
simple_hash(unsigned char *restrict dst, const unsigned char *restrict src, size_t len)
{
	unsigned char hbuf[32];
	int ret;

	ret = blake2b(hbuf, sizeof(hbuf), src, len, NULL, 0);
	assert(ret == 0);
	hash_to_base64(dst, hbuf);
}

static inline int
simple_hash_file(unsigned char *restrict dst, const char *fname)
{
	unsigned char dbuf[4096];
	ssize_t rd, off;
	blake2b_state S;
	int err;
	FILE *f;

	if ((f = fopen(fname, "r")) == NULL)
		return errno;

	err = blake2b_init(&S, 32);
	assert(err == 0);
	while ((rd = fread(dbuf, 1, sizeof(dbuf), f)) > 0) {
		err = blake2b_update(&S, dbuf, rd);
		assert(err == 0);
	}
	if (rd < 0)
		return errno;
	err = blake2b_final(&S, dbuf, 32);
	assert(err == 0);
	hash_to_base64(dst, dbuf);
	return 0;
}
