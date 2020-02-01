(import
  scheme
  (distill plan)
  (distill package)
  (distill base)
  (distill execline)
  (distill buildenv)
  (only (chicken string) conc))

(define lz4
  (let* ((version '1.9.2)
         (src     (remote-archive
                    (conc "https://github.com/lz4/lz4/archive/v" version ".tar.gz")
                    "uwHhgT74Tk7ds0TQeFZTodoI1_5IZsRnVRNNHi7ywlc=")))
    (lambda (conf)
      (make-package
        label:  (conc "lz4-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (let ((makeflags (map
                                   pair->string=
                                   (append (cc-env conf) (make-env conf)))))
                  (make-recipe
                    script: (execline*
                              (cd ,(conc "lz4-" version))
                              (importas -u "-i" nproc nproc)
                              (if ((make -j $nproc DESTDIR=/out PREFIX=/usr ,@makeflags install)))
                              ,@(strip-binaries-script conf))))))))
