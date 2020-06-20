(import
  scheme
  (srfi 88) ; keyword->string
  (chicken module)
  (only (chicken base) o)
  (distill text)
  (distill plan)
  (distill memo)
  (distill base)
  (distill package)
  (distill execline)
  (distill kvector)

  (pkg libexpat)
  (pkg pkgconf))

;; generate a cmake 'toolchain file' from a configuration
(define cmake-toolchain-file
  (memoize-one-eq
   (lambda (conf)
     (interned
      "/etc/cmake-target-toolchain"
      #o644
      (lambda ()
	(let ((setln (lambda (p)
		       (display "set(")
		       (display (car p))
		       (display " ")
		       ;; the rhs has to be "-quoted if
		       ;; it is multiple words
		       (if (list? (cdr p))
			   (write (join-with " " (cdr p)))
			   (display (cdr p)))
		       (display ")\n"))))
	  (for-each
	   setln
	   `((CMAKE_SYSTEM_NAME . Linux)
	     (CMAKE_CROSSCOMPILING . YES)
	     (CMAKE_SYSTEM_PROCESSOR . ,($arch conf)) ;; todo: match uname -m
	     (CMAKE_SYSROOT . ,($sysroot conf))
	     (CMAKE_STAGING_PREFIX . /out/usr)
	     (CMAKE_INSTALL_PREFIX . /usr)
	     (BUILD_SHARED_LIBS . OFF)
	     (CMAKE_AR . ,($AR conf))
	     (CMAKE_RANLIB . ,($RANLIB conf))
	     (CMAKE_C_COMPILER . ,($CC conf))
	     (CMAKE_C_FLAGS_INIT . ,($CFLAGS conf))
	     (CMAKE_CXX_COMPILER . ,($CXX conf))
	     (CMAKE_CXX_FLAGS_INIT . ,($CXXFLAGS conf))
	     (CMAKE_EXE_LINKER_FLAGS_INIT . ,($LDFLAGS conf))
	     (CMAKE_FIND_ROOT_PATH_MODE_PROGRAM . NEVER)
	     (CMAKE_FIND_ROOT_PATH_MODE_LIBRARY . ONLY)
	     (CMAKE_FIND_ROOT_PATH_MODE_INCLUDE . ONLY)
	     (CMAKE_FIND_ROOT_PATH_MODE_PACKAGE . ONLY)))))))))

(define (kvlist kv)
  (kvector-foldl kv (lambda (k v lst)
		      (if v
			  (cons (##sys#symbol->string k)
				(cons (if (list? v) (join-with " " v) v) lst))
			  lst))
		 '()))

(define cmake
  (cc-package
   "cmake" "3.17.2"
   "https://github.com/Kitware/CMake/releases/download/v$version/$name-$version.tar.gz"
   "34AhocPzPUZyt0Ic_2knEbFCKBGW1HCGJOebDceTi-0="
   use-native-cc: #t ;; for bootstrap
   libs: (list linux-headers zlib libexpat libressl libzstd libbz2)
   tools: (lambda (conf)
	    (list linux-headers (cmake-toolchain-file conf)))
   build: (elif*
	   `(exportall
	     ;; *just* for bootstrap, export BUILDCC et al. as CC, CFLAGS, etc.
	     (,(o kvlist cc-toolchain-env $build-toolchain))
	     ./bootstrap --prefix=/usr
	     --system-zlib
	     --system-expat
	     --system-zstd
	     --system-bzip2
	     --no-qt-gui --
	     ;; TODO: figure out how to use more system libs;
	     ;; --system-libarchive and --system-curl don't work
	     ;; because there isn't an (obvious?) way to add
	     ;; the necessary '-lssl -lcrypto' linker flags, etc.
	     "-DCMAKE_BUILD_TYPE:STRING=Release"
	     "-DCMAKE_TOOLCHAIN_FILE=/etc/cmake-target-toolchain")
	   '(make)
	   '(make install)
	   '(rm -rf /out/usr/doc))))

(export cmake-package)
(define (cmake-package name ver urlfmt hash
		       #!key (libs '()) (tools '()) (cleanup '()))
  (cc-package
   name ver urlfmt hash
   libs: libs
   tools: (lambda (conf)
	    (cons* pkgconf cmake (cmake-toolchain-file conf) tools))
   build: `(if
	    (mkdir builddir)
	    cd builddir
	    if (cmake "-DCMAKE_BUILD_TYPE:STRING=Release"
		      "-DCMAKE_TOOLCHAIN_FILE=/etc/cmake-target-toolchain"
		      "..")
	    if (make)
	    if (make install)
	    if (rm -rf /out/usr/share/man /out/usr/share/doc /out/usr/share/info)
	    ,@cleanup)))
