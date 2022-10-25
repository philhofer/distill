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
