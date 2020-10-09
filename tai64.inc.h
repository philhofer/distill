#include <time.h>
#include <stdint.h>
static const int64_t leapseconds[] = {
    63072000LL,
    78796800LL,
    94694400LL,
    126230400LL,
    157766400LL,
    189302400LL,
    220924800LL,
    252460800LL,
    283996800LL,
    315532800LL,
    362793600LL,
    394329600LL,
    425865600LL,
    489024000LL,
    567993600LL,
    631152000LL,
    662688000LL,
    709948800LL,
    741484800LL,
    773020800LL,
    820454400LL,
    867715200LL,
    915148800LL,
    1136073600LL,
    1230768000LL,
    1341100800LL,
    1435708800LL,
    1483228800LL,
};
#define LEAPS (sizeof(leapseconds)/sizeof(leapseconds[0]))

static int
subleap(int64_t sec)
{
    int i;

    for (i = LEAPS-1; i >= 0; i--) {
          if (sec >= leapseconds[i]+10LL+(long long)i) return sec - 10LL - (long long)i;
    }

    return sec;
}

static int
addleap(int64_t sec)
{
    int i;

    for (i = LEAPS-1; i >= 0; i--) {
        if (sec >= leapseconds[i]) return sec + 10LL + (long long)i;
    }
    return sec;
}

static void
tai64n_encode(int64_t sec, int32_t nsec, unsigned char *dst)
{
    dst[0] = ((sec >> 56) & 0xff);
    dst[1] = ((sec >> 48) & 0xff);
    dst[2] = ((sec >> 40) & 0xff);
    dst[3] = ((sec >> 32) & 0xff);
    dst[4] = ((sec >> 24) & 0xff);
    dst[5] = ((sec >> 16) & 0xff);
    dst[6] = ((sec >> 8)  & 0xff);
    dst[7] = ((sec >> 0)  & 0xff);
    dst[8] = ((nsec >> 24) & 0xff);
    dst[9] = ((nsec >> 16) & 0xff);
    dst[10] = ((nsec >> 8) & 0xff);
    dst[11] = ((nsec >> 0) & 0xff);
}

static int
tai64n_gettime(unsigned char *dst)
{
    struct timespec ts;
    int err;

    if ((err = clock_gettime(CLOCK_REALTIME, &ts)) < 0)
        return -errno;
    tai64n_encode(addleap(ts.tv_sec)+(1LL<<62), ts.tv_nsec, dst);
    return 0;
}
