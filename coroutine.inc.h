#include <stdbool.h>
#include <stdint.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/eventfd.h>
#include <err.h>
static int chldfd;
static void handle_sigchld(int sig)
{
    int64_t val;
    int rc;

    val = 1;
    do {
        rc = write(chldfd, &val, sizeof(val));
    } while (rc < 0 && errno == EINTR);
    /* terribly bad ... */
    if (rc != sizeof(val)) err(1, "write(eventfd)");
}
static int sigchld_handler(void)
{
    struct sigaction act;
    if ((chldfd = eventfd(0, EFD_CLOEXEC|EFD_NONBLOCK)) < 0)
        return -errno;

    act = (struct sigaction){
        .sa_handler = handle_sigchld,
        .sa_flags = SA_NOCLDSTOP,
    };
    if (sigaction(SIGCHLD, &act, NULL) < 0) return -errno;
    return chldfd;
}
static int do_poll(int32_t *rfd, int nrfd, int32_t *wfd, int nwfd, bool block)
{
    struct pollfd *pfd;
    int ret, i;

    pfd = calloc(nrfd+nwfd, sizeof(struct pollfd));
    if (!pfd) err(1, "out of memory");

    for (i=0; i<nrfd; i++) {
        pfd[i].fd = rfd[i];
        pfd[i].events = POLLIN|POLLERR|POLLHUP;
    }
    for (i=nrfd; i<nrfd+nwfd; i++) {
        pfd[i].fd = wfd[i-nrfd];
        pfd[i].events = POLLOUT|POLLERR|POLLHUP;
    }
again:
    ret = poll(pfd, nrfd+nwfd, block ? -1 : 0);
    if (!ret) goto done;
    if (ret < 0) {
        if ((errno == EAGAIN || errno == EINTR) && block) goto again;
        ret = block ? -errno : 0;
        goto done;
    }
    for (i=0; i<nrfd; i++) {
        if (!pfd[i].revents) rfd[i]=-1;
    }
    for (i=nrfd; i<nrfd+nwfd; i++) {
        if (!pfd[i].revents) wfd[i]=-1;
    }
done:
    free(pfd);
    return ret;
}
