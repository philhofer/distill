#ifndef ARCHIVE_H_
#define ARCHIVE_H_
#include <fcntl.h>
#include <err.h>
#include <dirent.h>
#include <archive.h>
#include <archive_entry.h>
#include "simple-hash.h"

static int
can_archive(const struct dirent *e)
{
    return strcmp(e->d_name, ".") &&
        strcmp(e->d_name, "..") &&
        (e->d_type == DT_DIR || e->d_type == DT_REG || e->d_type == DT_LNK);
}

/* each type accepts (real path, path in archive, archive ptr) */
static int archive_dir(const char *, const char *, struct archive *);
static int archive_reg(const char *, const char *, struct archive *);
static int archive_link(const char *, const char *, struct archive *);

static int
namecmp(const struct dirent **a, const struct dirent **b)
{
    return strcmp((*a)->d_name, (*b)->d_name);
}

static int
walk_dirent(const char *abs, const char *rel, const struct dirent *ent, struct archive *dst)
{
    int ret = 0;
    char *newrel, *newabs;

    if (asprintf(&newabs, "%s/%s", abs, ent->d_name) < 0)
        err(1, "asprintf");
    if (asprintf(&newrel, "%s/%s", rel, ent->d_name) < 0)
        err(1, "asprintf");
    switch (ent->d_type) {
    case DT_DIR:
        ret = archive_dir(newabs, newrel, dst);
        break;
    case DT_REG:
        ret = archive_reg(newabs, newrel, dst);
        break;
    case DT_LNK:
        ret = archive_link(newabs, newrel, dst);
        break;
    }
    free(newrel);
    free(newabs);
    return ret;
}

static int
archive_link(const char *abs, const char *rel, struct archive *dst)
{
    struct archive_entry *ent;
    struct stat stat;
    char linkbuf[1024];

    int rc = readlink(abs, linkbuf, sizeof(linkbuf)-1);
    if (rc < 0)
        err(1, "readlinkat %s", abs);
    if (rc == sizeof(linkbuf)-1)
        errx(1, "link for %s too long", abs);
    linkbuf[rc] = 0;

    ent = archive_entry_new();
    if (!ent)
        err(1, "archive_entry_new");
    archive_entry_copy_pathname(ent, rel);
    archive_entry_set_filetype(ent, AE_IFLNK);
    archive_entry_copy_symlink(ent, linkbuf);
    archive_entry_unset_atime(ent);
    archive_entry_unset_mtime(ent);
    archive_entry_unset_ctime(ent);
    archive_entry_set_uid(ent, 0);
    archive_entry_set_gid(ent, 0);
    archive_write_header(dst, ent);

    archive_entry_free(ent);
    return 0;
}

static int
archive_reg(const char *abs, const char *rel, struct archive *dst)
{
    int fd;
    ssize_t nb;
    struct archive_entry *ent;
    struct stat stbuf;
    char tmpbuf[4096];

    fd = open(abs, O_RDONLY|O_CLOEXEC);
    if (fd < 0)
        err(1, "open %s", abs);
    if (fstat(fd, &stbuf) < 0)
        err(1, "stat %s", abs);
    if (!S_ISREG(stbuf.st_mode))
        errx(1, "file %s not a regular file? (race condition?)");

    ent = archive_entry_new();
    if (!ent)
        err(1, "archive_entry_new");

    archive_entry_copy_pathname(ent, rel);
    archive_entry_set_filetype(ent, AE_IFREG);
    archive_entry_set_mode(ent, stbuf.st_mode);
    archive_entry_set_size(ent, stbuf.st_size);
    archive_entry_unset_atime(ent);
    archive_entry_unset_mtime(ent);
    archive_entry_unset_ctime(ent);
    archive_entry_set_uid(ent, 0);
    archive_entry_set_gid(ent, 0);

    archive_write_header(dst, ent);

    while ((nb = read(fd, tmpbuf, sizeof(tmpbuf))) > 0)
        archive_write_data(dst, tmpbuf, nb);

    close(fd);
    archive_entry_free(ent);
    return 0;
}

static int
archive_dir(const char *abs, const char *rel, struct archive *dst)
{
    int n, rc, i;
    struct stat stbuf;
    struct dirent **namelist;
    struct archive_entry *ent;

    if (stat(abs, &stbuf) < 0)
        err(1, "stat %s", abs);
    if (!S_ISDIR(stbuf.st_mode))
        errx(1, "stat %s is not a directory?", abs);
    ent = archive_entry_new();
    if (!ent)
        err(1, "archive_entry_new");
    archive_entry_copy_pathname(ent, rel);
    archive_entry_set_filetype(ent, AE_IFDIR);
    archive_entry_set_mode(ent, stbuf.st_mode);
    archive_entry_unset_atime(ent);
    archive_entry_unset_mtime(ent);
    archive_entry_unset_ctime(ent);
    archive_entry_set_uid(ent, 0);
    archive_entry_set_gid(ent, 0);
    archive_write_header(dst, ent);

    archive_entry_free(ent);

    n = scandir(abs, &namelist, &can_archive, &namecmp);
    if (n < 0)
        return n;
    for (i = 0; i < n; i++) {
        if (rc >= 0)
            rc = walk_dirent(abs, rel, namelist[i], dst);
        free(namelist[i]);
    }
    free(namelist);
    return rc;
}

struct archive_ctx {
    int dstfd;
    blake2b_state hash;
};

static int
open_cb(struct archive *a, void *p)
{
    return ARCHIVE_OK;
}

static ssize_t
write_cb(struct archive *a, void *p, const void *buffer, size_t len)
{
    struct archive_ctx *ctx = p;
    int rc;

    blake2b_update(&ctx->hash, buffer, len);
    return write(ctx->dstfd, buffer, len);
}

static int
close_cb(struct archive *a, void *p)
{
    struct archive_ctx *ctx = p;
    close(ctx->dstfd);
    return ARCHIVE_OK;
}

static int
free_cb(struct archive *a, void *p)
{
    return ARCHIVE_OK;
}

static int
make_tar_zst(const char *from, const char *to, char *dsthash)
{
    int rc, dstfd;
    struct archive *a;
    struct archive_ctx ctx = {0};
    unsigned char rawhash[32];

    dstfd = open(to, O_WRONLY|O_CREAT|O_TRUNC|O_CLOEXEC, 0644);
    if (dstfd < 0)
        err(1, "open %s", to);

    ctx.dstfd = dstfd;
    blake2b_init(&ctx.hash, sizeof(rawhash));

    a = archive_write_new();
    if (!a)
        err(1, "archive_write_new");
    archive_write_set_format_ustar(a);
    archive_write_add_filter_zstd(a);
    archive_write_set_option(a, "Filter zstd", "compression-level", "7");
    archive_write_set_bytes_in_last_block(a, 1);
    archive_write_open2(a, &ctx, &open_cb, &write_cb, &close_cb, &free_cb);

    rc = archive_dir(from, ".", a);
    if (rc == 0)
        rc = archive_write_close(a);
    else
        unlink(to);
    archive_write_free(a);
    if (rc == 0) {
        blake2b_final(&ctx.hash, rawhash, sizeof(rawhash));
        hash_to_base64(dsthash, rawhash);
    }
    close(dstfd);
    return rc;
}

#endif // ARCHIVE_H_
