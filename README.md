# User Guide

## Introduction

Distill is a tool for specifying and (hermetically!) building
reproducible and cross-compile-able Linux disk images.
The distill tool itself is a scheme interpreter
that turns scheme scripts into bootable disk images.

In a rudimentary sense, distill is a declaratively-configured
Linux distribution, although unlike Nix or Guix, the engineering focus is
on building immutable images that run *elsewhere* rather than
trying to edit the configuration of ones own machine in a way
that minimizes the interference bewteen installed software.

The package recipes that come bundled with the `distill` tool
go to great lengths to ensure that each build is reproducible
and supports cross-compilation. (Build dependencies are structured
so that there is a clear distinction between build and host dependencies.)

## High-level: Packages, Services, Systems, and Platforms

### System

A "system" is the term distill uses for describing
a complete bootable system. Here's a minimal system definition
that includes `sshd(8)`, `chrony(8)`, and `dhcpcd(8)` in order
to have the machine bring up networking via DHCP and allow login
via an SSH certificate.

```
(import
  scheme
  (distill plan)   ; interned
  (distill system) ; make-system
  
  ;; import some package definitions
  (pkg ca-certs)
  (pkg wget)
  
  ;; import some service definitions
  (svc sshd)
  (svc net-dhcpcd)
  (svc chronyd))

;; define a raw text file that
;; should live at /etc/ssh/ca.pub;
;; this is the SSH CA that determines who can log in
(define my-ssh-ca
  (interned
  "/etc/ssh/ca.pub" #o600
  (lines
    '((ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJuTFM6dNs7O6w5ottv5Ri+aABOstF/31UO31r+pCl/L6afDQP0D2NQmreJAmqCU98ewITyBkRBuSXx9BKaTX7Y=)))))

;; system is the system definition;
;; it includes
(define system
  (make-system
   services: (list
               ;; use dhcpcd(8) for networking
               (net-dhcpcd default-dhcpcd-config)
               ;; use chronyd(8) as a clock source
               (chronyd default-chronyd-config)
               ;; use sshd(8) and accept pubkeys signed by our CA
               (sshd '((TrustedUserCAKeys /etc/ssh/ca.pub))))
   packages: (list wget ca-certs)))
```

### Platform

A "platform" is a set of packages, base services,
and kernel / root filesystem configuration necessary
to bring up a "system" on a certain set of hardware.

Platform definitions have the following minimum responsibilities:

 - Provide a config for packages (architecture, any required `CFLAGS`, etc.)
 - Provide a bootloader (and any necessary firmware) for booting
 - Provide a read/write mount on `/var` as one of the base services.

Additionally, most platforms also provide:

 - ACPI power management via `acpid(8)`
 - A `getty` attached to one or more serial consoles (including
 on cloud platforms that provide out-of-band serial console access)

Examples of platforms are Nitro EC2 x86_64 (`plat/nitro-ami-x86_64.scm`),
Digital Ocean Droplets (`plat/droplet.scm`), or the Solid Run Macchiatobin
embedded platform (`plat/mcbin-sdimage.scm`).

For virtualized and x86_64 systems, we currently
use a minimal BIOS bootloader.
Embedded platforms typically use uboot.

Since platform definitions are simply data structures,
it is straightforward to write one platform definition
in terms of another one (in order to replace the kernel,
adjust base services, etc.)

### Services

A "service" is either a daemon or one-shot script
that is run automatically at boot time.
Service definitions typically indicate one or
more prerequisite packages that contain the tools
necessary to run the service, plus the actual
script for executing the service.

The primary entrypoint for defining services is
via `make-service` in `(distill service)`.
Distill images use [s6-rc](https://skarnet.org/software/s6-rc)
for service management and [s6](https://skarnet.org/software/s6)
for process management.

The `distill` tool comes with a number of built-in
services that can be imported via `(svc ...)`; they live
in `svc/*.scm` in the source tree.

TODO: document `make-service`
(For now read examples in `svc/*.scm`)

### Packages

A "package" is a function that accepts a "config"
and returns a "plan." (See "Artifacts and Plans" below.)
More abstractly, a package is a scheme object that
represents a collection of filesystem objects that may
require a build step in order to produce. (For example,
`gcc` and `wget` and `curl` are all packages.)

Since many open-source software packages use
(somewhat) standardized build systems, there are helpers in
`(distill package)` for writing terse definitions of packages
that adhere reasonably well to one of these conventions
(the most common being `./configure; make; make install`).

For example, here's the package definition for `libmpfr`:
```
(define libmpfr
  (cmmi-package
   "mpfr" "4.1.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.bz2"
   "72Rb7WvnPA9a0c4r-UdkHoHqzubqlKae8x40RrGulfM="
   libs: (list libgmp)))
```

The `cmmi-package` function is a helper for defining
packages that use the aforementioned "autotools"-style
build system. The long base64 string on its own line
is the BLAKE2b-256 hash of the tarball that ought
to be downloaded from the URL on the previous line.
(All remote source tarballs are checked against pinned
content hashes in order to prevent reproducibility problems
or supply-chain attacks.)

The `libs:` keyword argument in the package
definition indicates that this package depends on `libgmp`
in order to build. (The `cmmi-package` function takes care
of pulling in a C compiler and libc.)

See the docs for `(distill package)` for more details,
and core packages in `base.scm` and `pkg/*.scm`.

## Conventions

### Execline

Build scripts in distill are written in the [execline](https://skarnet.org/software/execline) language.
The execline language is particularly easy to manipulate in
Scheme because it has almost zero syntax. We represent
an execline program as a simple list of scheme datums,
where each execline "block" is simply a sub-list.

Here's an example scheme datum
and the corresponding execline text that
would be generated from it:
```
# scheme:
`(if (sed -e s/foo/bar/g "s/foo = bar/bar = foo/g" file)
      ifelse (echo #u8(127 69 76 70)) (echo "echo failed")
      if (sysctl -p ,conf)
      forbacktickx file (pipeline
                         (elglob extra "/etc/sysctl.d/*.conf" echo $extra)
                         sort)
      importas file f
      echo $f))

# execline:
#!/bin/execlineb -P
if {
	sed -e s/foo/bar/g "s/foo = bar/bar = foo/g" file
}
ifelse {
	echo "\0x7fELF"
} {
	echo "echo failed"
}
if {
	sysctl -p /etc/sysctl.conf
}
forbacktickx file {
	pipeline {
		elglob extra /etc/sysctl.d/*.conf echo $extra
	} sort
}
importas file f echo $f
```

By convention, execline scripts use "Config Expansion" (see below)
for interpolating values that depend on architecture-specific configuration.
There are a variety of helper functions in `(distill execline)`
for constructing execline scripts that require special configuration expansion.
For example, `(elconc "foo" $CC "bar")`, which concatenates its expanded
arguments into a string, would expand to `"foogccbar"` when `$CC` expands to `"gcc"`.
Under the hood, config "variables" are just procedures, and "Config Expansion"
is simply the process of traversing a script and calling any procedures in the
script with the current configuration as the only argument, taking care to splice
the result of the procedure into the script as the final result.

### Config Expansion

In most of the package helper functions in `(distill package)`,
any procedures that occur within a lisp datum (pair, list, etc.)
are assumed to be functions of one argument that accept the current
`config` variable and return the value to be spliced into the list.
At package-resolution time, these procedures are iteratively expanded
until they produce non-procedure values.
By convention, these functions have names that begin with `$`.

For example, here's the definition of `libgmp`:
```
(define libgmp
  (cmmi-package
   "gmp" "6.2.1"
   "https://gmplib.org/download/gmp/gmp-$version.tar.xz"
   "W4qlaa9eWffZEteaNvBOdJgw75jYF5Y1I3HitfZ9Pfw="
   ;; gmp's configure script ignores CFLAGS_FOR_BUILD,
   ;; so we have to shove everything into CC_FOR_BUILD
   env: `((CC_FOR_BUILD ,$build-CC ,$build-CFLAGS))
   tools: (list m4 native-toolchain)
   extra-configure: '(--with-pic)))
```

The values `$build-CC` and `$build-CFLAGS` are
functions that extract the right `CC` and `CFLAGS`
values when building binaries to run on the *build* architecture.

## Low-level: Artifacts and Plans

Artifacts are what distill calls any blob of data
with an associated content hash. Images are built
by building a dependency graph of "plans" and then
resolving each plan to an artifact.

A plan is a collection of input artifacts
and their respective locations in a fresh filesystem hierarchy.
Every plan must include an executable at `/build` that produces
the plan outputs at puts them in `/out`.
The tool keeps track of the set of input and output objects for
every plan that is built successfully in order to avoid
rebuilding identical plans.

The distill library functions for declaring packages,
services, etc. are just wrappers around the core
"plan" metaphor, although the `(distill plan)` module
exposes these low-level primitives for direct manipulation.
# Module Reference
## module `(distill base)`
### default-build-config
```
(define default-build-config ...)
```
set package#build-config to
the default config for this machine

### default-config
```
(define (default-config arch) ...)
```
default-config produces the default config for 'arch'
which should be one of '(x86_64 aarch64 ppc64le)

### busybox-full
```
(define busybox-full ...)
```
busybox with additional stuff built in

### hard
```
(define hard ...)
```
hard(8) command; an alternative to busybox halt(8)/reboot(8)/poweroff(8)

### uboot/config
```
(define (uboot/config name hash env bootcmd) ...)
```
uboot/config accepts 4 arguments:
 - name: a suffix added to the package name
   (the package will be named "uboot-<version>-<name>"
 - hash: the hash of the .config
 - env:  a list of key=value strings that populate
   the default environment for the bootloader
 - bootcmd: the default kernel boot command for u-boot (i.e. booti, etc.)

### libelf
```
(define libelf ...)
```
libelf is *just* libelf.a and headers;
it does not include the rest of elfutils

### linux/config-static
```
(define (linux/config-static
         name
         config-path
         #!key
         (install *default-installkernel*)
         (dtb #f))
  ...)
```
linux/config takes a name and configuration hash
and produces a kernel package named "linux-<version>-<name>"
using the given configuration file

### busybox-core
```
(define busybox-core ...)
```
busybox-core is just enough busybox to build packages;
it doesn't include system utilities that would require
linux headers

### libisl
```
(define libisl ...)
```
NOTE: the latest version of isl is 0.22.1, but
it is not available through any HTTPS mirror that I have found...
the gcc infrastructure mirror only includes up to 0.18

### exportall
```
(define exportall ...)
```
exportall(1) is an execline tool for exporting a block of variables
(in execline syntax, it works like 'exportall { key val ... } prog ...'
so you'd write (exportall ((k v) ...)) in a build script

### libssp-nonshared
```
(define libssp-nonshared ...)
```
static library that defines the __stack_chk_fail_local symbol

### cc-for-target
```
(define (cc-for-target conf #!optional (native #f)) ...)
```
cc-for-target produces a list of packages
that together constitute a C/C++ toolchain
for a particular target; if native is supplied
and not #f, then symlinks and packages for
the a C/C++ toolchain for the build configuration
are also included

### gcc+musl-static-config
```
(define (gcc+musl-static-config
         arch
         #!key
         (optflag '-O2)
         (sspflag '-fstack-protector-strong)
         (extra-cflags '())
         (build #f)
         (bootstrap #f)
         (prebuilt #f))
  ...)
```
gcc+musl-static-config produces a config that uses
gcc and musl libc for static linking

### gcc
```
(define gcc ...)
```
gcc is a pseudo-package that provides
a gcc for build=host plus wrappers that
provide /usr/bin/gcc, etc

### cdn-artifact
```
(define (cdn-artifact hash abspath mode) ...)
```
stuff like default busybox configs, etc.
are stored remotely and fetched on-demand

## module `(distill package)`
### patchfiles*
```
(define (patchfiles* . names) ...)
```
patchfiles* looks relative to the current directory,
the current executable's directory, and finally
the installed lib directory for files with the given
names to be used as patches

### bind
```
(define (bind herepath therepath) ...)
```
bind locates a file relative to the current working directory,
the current install directory, or the current install's lib directory,
and produces an artifact that appears at 'therepath' with the same
permissions as the file has locally

### config->builder
```
(define (config->builder env) ...)
```
config->builder takes a configuration (as an alist or conf-lambda)
and returns a function that builds the package
and yields its output artifact

### $cc-env/for-kbuild
```
(define ($cc-env/for-kbuild conf) ...)
```
cc-env/for-kbuild is a C environment
with HOSTCC, HOSTCFLAGS, etc.
(this is typical for Kbuild-based build systems)

### $cc-env/for-build
```
(define ($cc-env/for-build conf) ...)
```
cc-env/for-build is a C environment
with CC_FOR_BUILD, CFLAGS_FOR_BUILD, etc.
(this is typical for autotools-based configure scripts)

### cc-env/build
```
(: cc-env/build (vector (keyword -> keyword) -> vector))
(define (cc-env/build conf frob-kw) ...)
```
cc-env/build produces a C environment
by transforming the usual CC, LD, etc.
variables using (frob-kw key) for each keyword

### $make-overrides
```
(define $make-overrides ...)
```
$make-overrides is the subset of <config>
that is supplied as k=v arguments to invocations
of $MAKE (in order to override assignments in the Makefile)

### cmmi-package
```
(define (cmmi-package
         name
         version
         urlfmt
         hash
         #!key
         (dir #f)
         (patches '())
         (env '())
         (extra-configure '())
         (override-configure #f)
         (override-make #f)
         (override-install #f)
         (out-of-tree #f)
         (libs '())
         (tools '())
         (cross '())
         (extra-src '())
         (prepare #f)
         (cleanup #f)
         (native-cc #f)
         (parallel #t)
         (extra-cflags '()))
  ...)
```
template for 'configure; make; make install;' or 'c;m;mi'
that has sane defaults for environment, tools, libraries
(see cc-package) and a reasonable default for the build script.

### cc-package
```
(define (cc-package
         name
         version
         urlfmt
         hash-or-art
         #!key
         (dir #f)
         (build #f)
         (no-libc #f)
         (use-native-cc #f)
         (raw-output #f)
         (patches '())
         (env '())
         (libs '())
         (tools '())
         (cross '())
         (extra-src '()))
  ...)
```
cc-package is a template for C/C++ packages

### url-translate
```
(define (url-translate urlfmt name version) ...)
```
url-translate takes a url string in urlfmt
and a name and version string to substitute
into the literal substrings "$name" and "$version"
in urlfmt

### binaries
```
(define (binaries pkg) ...)
```
binaries produces a subpackage
by matching common binary directories:
 - /usr/bin
 - /bin
 - /usr/sbin
 - /sbin
 - /usr/libexec
 - /libexec
 - /usr/share
 - /share

### libs
```
(define (libs pkg) ...)
```
libs wraps a package and yields
only the parts of its outputs that
are in conventional locations for C library files:
 - /usr/lib
 - /lib
 - /usr/include
 - /include
 - /usr/share

### subpackage
```
(: subpackage (string * #!rest string -> (vector -> vector)))
(define (subpackage prefix sub . dirs) ...)
```
subpackage takes a package definition
and returns a subset of the output artifacts
of that package.

### expander
```
(: expander (vector -> (* -> *)))
(define (expander host) ...)
```
expander returns a monoid that expands
lambdas into plan or artifact objects

### configure
```
(: configure (* vector -> *))
(define (configure x conf) ((%current-expander) x conf))
```
(configure x conf) expands x with 'conf'
using the current expansion environment

### package-template
```
(define (package-template
         #!key
         label
         build
         (src '())
         (cross '())
         (tools '())
         (inputs '())
         (patches '())
         (dir "/")
         (env '())
         (raw-output #f))
  ...)
```
package-template is a helper
for writing package definitions.

### *this-machine*
```
(: *this-machine* symbol)
(define *this-machine* ...)
```
*this-machine* is the architecture
for which the currently-running program
was compiled

## module `(distill eprint)`
## module `(distill fetch)`
### fetch
```
(: fetch (string string -> *))
(define (fetch url dst) ...)
```
from the given url and puts it in the given file;
presently this is implemented by exec-ing wget
or curl depending on which one happens to be
available

### user-fetch-hook
```
(define user-fetch-hook (make-parameter #f))
```
user-fetch-hook can be used to
pick the URL used to fetch an artifact,
or it can fetch an artifact all on its own.

### symlink-from-directory
```
(define (symlink-from-directory dir) ...)
```
symlink-from-directory can be supplied
to user-fetch-hook to cause objects to be
"fetched" from another directory by simply
symlinking the target artifacts into the
destination

## module `(distill fs)`
### kmsg
```
(define kmsg ...)
```
kmsg is a super lightweight syslogd-style service
that reads logs from /dev/kmsg and stores them in
/var/log/services/kmsg, taking care to compress
(and eventually delete) old logs

### var-mount
```
(define (var-mount dev) ...)
```
var-mount creates a read-write mount at /var

### var-mounted-rw
```
(define var-mounted-rw 'var-mount)
```
logging services will generally depend on var-mounted-rw

### swapon
```
(define (swapon dev) ...)
```
swap is a service that uses 'dev' for swap

## module `(distill hash)`
### hash-file
```
(: hash-file (string -> (or string false)))
(define (hash-file fp) ...)
```
hash-file returns the hash of a file
(if the file exists), or #f if the file
doesn't exist, or it throws an error if
an I/O error is encountered

### zero-hash
```
(: zero-hash string)
(define zero-hash (hash-of ""))
```
zero-hash is the hash of zero bytes of data

### copy-port+hash
```
(: copy-port+hash (input-port output-port -> string))
(define (copy-port+hash from to) ...)
```
copy-port+hash copies port 'from' to port 'to'
and yields the hash of the copied data

### with-output-to-hash
```
(: with-output-to-hash ((-> *) -> string))
(define (with-output-to-hash thunk) ...)
```
with-output-to-hash calls (thunk) with
current-output-port set to a hasher;
the returned value is the hash of the
accumulated output

### hasher->output-port
```
(: hasher->output-port (u8vector -> output-port))
(define (hasher->output-port h) ...)
```
hasher->output-port takes a hashing object
and produces an output port that calls
hash-write! when data is sent to the port

## module `(distill image)`
### efi-image
```
(define (efi-image name #!key (uuid #f)) ...)
```
esp-image produces an EFI-bootable image

### mbr-image
```
(define (mbr-image name) ...)
```
mbr-image produces a (legacy-)bootable image

### linux-esp
```
(define (linux-esp kernel bootargs) ...)
```
linux-esp creates an ESP ("EFI system partition")
that should boot into the given kernel (package) with
the given boot arguments

### ext2fs
```
(define (ext2fs name uuid . inputs) ...)
```
ext2fs creates a package-lambda that
takes everything in 'inputs' and produces
an ext2 filesystem image (as a sparse file)

### squashfs
```
(define (squashfs inputs chown #!key (compress 'zstd)) ...)
```
squashfs produces a squashfs root filesystem from a set of inputs

## module `(distill net)`
## module `(distill plan)`
### load-plan
```
(: load-plan (string -> vector))
(define (load-plan hash) ...)
```
load-plan loads an old plan from the plan directory

### build-plan!
```
(: build-plan! (vector -> artifact))
(define (build-plan! top) ...)
```
build-plan! unconditionally builds a plan
and produces its output artifact
(it does not save the output)

### for-each-anchor
```
(define (for-each-anchor proc plst) ...)
```
for-each-anchor traverses a list of plans and artifacts
and recursively applies (proc artifact) to each artifact
that is a leaf node of the DAG

### live-artifact-hashes
```
(define (live-artifact-hashes plst) ...)
```
live-artifact-hashes walks a list of
plans and artifacts and produces a hash table
of artifacts involved in those plans that are actually live

### plan-outputs
```
(: plan-outputs (vector -> (or artifact false)))
(define (plan-outputs p) ...)
```
determine the outputs (leaf) of the given plan,
or #f if the plan has never been built with its inputs

### plan-hash
```
(: plan-hash (vector -> (or false string)))
(define (plan-hash p) ...)
```
plan-hash returns the canonical hash of a plan,
or #f if any of its inputs have unknown outputs

### plan-dir
```
(define plan-dir ...)
```
by default, dump stuff into these directories

### interned-symlink
```
(: interned-symlink (string string --> vector))
(define (interned-symlink abspath lnk) ...)
```
interned-symlink creates a link at 'abspath'
that points to 'lnk'

### remote-file
```
(: remote-file ((or false string) string string fixnum --> artifact))
(define (remote-file src hash abspath mode) ...)
```
remote-file creates a new file artiact
at the given absolute path with the given file permissions
from the source url and hash provided

### update-path
```
(: update-path (* string -> (or artifact false)))
(define (update-path art newpath) ...)
```
update-path takes a file artifact
and returns a new artifact with
an updated output path

### remote-archive
```
(: remote-archive (string string #!rest * --> artifact))
(define (remote-archive src hash #!key (kind #f)) ...)
```
TODO: normalize remote archives for faster re-builds
(large xz tarballs are reeeeally slow to unpack)

### short-hash
```
(define (short-hash h) ...)
```
short-hash is a 6-character hash prefix

## module `(distill text)`
### map-lines
```
(define (map-lines proc lst) ...)
```
map-lines is equivalent to (lines (map proc lst))

### lines
```
(define (lines lst) ...)
```
(lines lst) produces a string with each
element of 'lst' on its own line, with
sub-lists interspersed with spaces

### tabular
```
(define (tabular op wsep lsep lst) ...)
```
tabular displays each element of (map op lst)
using wsep as the field separator and lsep
as the row separator

### join-with
```
(define (join-with sep lst) ...)
```
join-with produces a string
by concatenating each element
of 'lst' interspersed with 'sep'

## module `(distill tai64)`
### tai64n-nanoseconds
```
(: tai64n-nanoseconds (u8vector --> integer))
(define (tai64n-nanoseconds tm) (decode-int tm 8 12))
```
tai64n-nanoseconds yields the
nanoseconds component of the tai64n object

### tai64n-seconds
```
(: tai64n-seconds (u8vector --> integer))
(define (tai64n-seconds tm) (decode-int tm 0 8))
```
tai64n-seconds yields the seconds component
of the tai64n object

### unix->tai64n
```
(: unix->tai64n (integer -> u8vector))
(define (unix->tai64n secs) ...)
```
unix->tai64n converts a unix epoch timestamp
(as an integer) into a TAI64N timestamp

### tai64n-now
```
(: tai64n-now (-> u8vector))
(define (tai64n-now) ...)
```
tai64n-now produces the current time
as a TAI64N timestamp

## module `(distill unix)`
### groups+users->artifacts
```
(define (groups+users->artifacts gps ups #!key (start-uid 100) (start-gid 100))
  ...)
```
groups+users->artifacts takes a list
of addgroup expressions and a list of
adduser expressions and returns a list
of artifacts for /etc/passwd, /etc/group, and so forth

## module `(distill system)`
## module `(distill filepath)`
### pathfind
```
(: pathfind (string -> (or string false)))
(define (pathfind bin) ...)
```
pathfind finds a binary in $PATH or returns #f

### folddir
```
(: folddir ((string 'a -> 'a) 'a string -> 'a))
(define (folddir proc seed path) ...)
```
folddir folds (proc file seed) over
every file under the directory 'path'
in strcmp-sorted order (but omitting
the "." and ".." entries)

### abspath
```
(: abspath (string --> string))
(define (abspath p) ...)
```
convert a relative path to an absolute path
if it is not one already (by prepending the current directory)

### filepath-join
```
(: filepath-join (stringy #!rest stringy --> string))
(define filepath-join ...)
```
core filepath normalization routine

### string-suffix?
```
(: string-suffix? (string string --> boolean))
(define (string-suffix? suff str) ...)
```
see srfi 13

### string-prefix?
```
(: string-prefix? (string string --> boolean))
(define (string-prefix? pre str) ...)
```
see srfi 13

