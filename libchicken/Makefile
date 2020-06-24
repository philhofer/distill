UNITS:=library eval read-syntax repl \
	data-structures pathname port file \
	extras lolevel srfi-4 continuation \
	posixunix internal irregex stub \
	expand modules chicken-syntax chicken-ffi-syntax \
	build-version eval-modules runtime

CHICKEN_OPTIONS:=-optimize-level 2 -explicit-use -no-trace -include-path . \
		-feature chicken-bootstrap -inline -specialize -disable-interrupts \
		-ignore-repository -consult-types-file types.db

.SUFFIXES:
%.o: %.c
	$(CC) $(NEEDED_CFLAGS) $(CFLAGS) -c $< -o $@

libchicken.a: ${UNITS:%=%.o}
	$(AR) cr $@ $^

# The rest of this Makefile is just cut+paste
# from the rules.make file in chicken-core.git

bootstrap-lib = $(CHICKEN) $< $(CHICKEN_FEATURES) $(CHICKEN_OPTIONS) -output-file $@

define declare-emitted-import-lib-dependency
.SECONDARY: $(1).import.scm
$(1).import.scm: $(2).c
endef

define declare-emitted-chicken-import-lib-dependency
$(call declare-emitted-import-lib-dependency,chicken.$(1),$(1))
endef

$(foreach lib, $(UNITS), \
	$(eval $(call declare-emitted-chicken-import-lib-dependency,$(lib),$(lib))))

POSIXFILE:=posixunix
# special cases for modules not corresponding directly to units
$(eval $(call declare-emitted-import-lib-dependency,chicken.errno,$(POSIXFILE)))
$(eval $(call declare-emitted-import-lib-dependency,chicken.file.posix,$(POSIXFILE)))
$(eval $(call declare-emitted-import-lib-dependency,chicken.time.posix,$(POSIXFILE)))
$(eval $(call declare-emitted-import-lib-dependency,chicken.process,$(POSIXFILE)))
$(eval $(call declare-emitted-import-lib-dependency,chicken.process.signal,$(POSIXFILE)))
$(eval $(call declare-emitted-import-lib-dependency,chicken.process-context.posix,$(POSIXFILE)))
$(eval $(call declare-emitted-import-lib-dependency,chicken.bitwise,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.blob,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.fixnum,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.flonum,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.gc,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.keyword,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.platform,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.plist,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.process-context,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.time,library))
$(eval $(call declare-emitted-import-lib-dependency,chicken.load,eval))
$(eval $(call declare-emitted-import-lib-dependency,chicken.format,extras))
$(eval $(call declare-emitted-import-lib-dependency,chicken.io,extras))
$(eval $(call declare-emitted-import-lib-dependency,chicken.pretty-print,extras))
$(eval $(call declare-emitted-import-lib-dependency,chicken.random,extras))
$(eval $(call declare-emitted-import-lib-dependency,chicken.locative,lolevel))
$(eval $(call declare-emitted-import-lib-dependency,chicken.memory,lolevel))
$(eval $(call declare-emitted-import-lib-dependency,chicken.memory.representation,lolevel))
$(eval $(call declare-emitted-import-lib-dependency,chicken.sort,data-structures))
$(eval $(call declare-emitted-import-lib-dependency,chicken.string,data-structures))

srfi-4.import.scm: srfi-4.c

modules.c: modules.scm \
		chicken.internal.import.scm \
		chicken.keyword.import.scm \
		chicken.base.import.scm \
		chicken.syntax.import.scm \
		chicken.load.import.scm \
		chicken.platform.import.scm
srfi-4.c: srfi-4.scm \
		chicken.bitwise.import.scm \
		chicken.foreign.import.scm \
		chicken.gc.import.scm \
		chicken.platform.import.scm
posixunix.c: posixunix.scm \
		chicken.bitwise.import.scm \
		chicken.condition.import.scm \
		chicken.foreign.import.scm \
		chicken.memory.import.scm \
		chicken.pathname.import.scm \
		chicken.platform.import.scm \
		chicken.port.import.scm \
		chicken.process-context.import.scm \
		chicken.time.import.scm
data-structures.c: data-structures.scm \
		chicken.base.import.scm \
		chicken.fixnum.import.scm \
		chicken.condition.import.scm \
		chicken.foreign.import.scm
expand.c: expand.scm \
		chicken.blob.import.scm \
		chicken.condition.import.scm \
		chicken.keyword.import.scm \
		chicken.platform.import.scm \
		chicken.internal.import.scm
extras.c: extras.scm \
		chicken.string.import.scm \
		chicken.time.import.scm
eval.c: eval.scm \
		chicken.blob.import.scm \
		chicken.condition.import.scm \
		chicken.foreign.import.scm \
		chicken.internal.import.scm \
		chicken.keyword.import.scm \
		chicken.fixnum.import.scm \
		chicken.platform.import.scm \
		chicken.time.import.scm
repl.c: repl.scm \
		chicken.base.import.scm \
		chicken.load.import.scm \
		chicken.foreign.import.scm \
		chicken.eval.import.scm
file.c: file.scm \
		chicken.condition.import.scm \
		chicken.io.import.scm \
		chicken.irregex.import.scm \
		chicken.foreign.import.scm \
		chicken.pathname.import.scm \
		chicken.process-context.import.scm
lolevel.c: lolevel.scm \
		chicken.base.import.scm \
		chicken.fixnum.import.scm \
		chicken.foreign.import.scm
pathname.c: pathname.scm \
		chicken.base.import.scm \
		chicken.fixnum.import.scm \
		chicken.irregex.import.scm\
		chicken.platform.import.scm \
		chicken.string.import.scm
port.c: port.scm \
		chicken.base.import.scm \
		chicken.fixnum.import.scm \
		chicken.foreign.import.scm \
		chicken.io.import.scm
read-syntax.c: read-syntax.scm \
		chicken.base.import.scm \
		chicken.internal.import.scm \
		chicken.platform.import.scm
#eval-modules.c: eval-modules.scm $(DYNAMIC_IMPORT_LIBRARIES:=.import.scm) \
#	$(foreach lib,$(DYNAMIC_CHICKEN_IMPORT_LIBRARIES),chicken.$(lib).import.scm) \
#	$(foreach lib,$(DYNAMIC_CHICKEN_UNIT_IMPORT_LIBRARIES),$(lib).c)
library.c: library.scm chicken.base.import.scm \
		chicken.foreign.import.scm
	$(bootstrap-lib) \
	-no-module-registration \
	-emit-import-library chicken.bitwise \
	-emit-import-library chicken.blob \
	-emit-import-library chicken.fixnum \
	-emit-import-library chicken.flonum \
	-emit-import-library chicken.gc \
	-emit-import-library chicken.keyword \
	-emit-import-library chicken.platform \
	-emit-import-library chicken.plist \
	-emit-import-library chicken.process-context
internal.c: internal.scm mini-srfi-1.scm \
		chicken.fixnum.import.scm
	$(bootstrap-lib) -emit-import-library chicken.internal
eval.c: eval.scm common-declarations.scm mini-srfi-1.scm
	$(bootstrap-lib) \
	-emit-import-library chicken.eval \
	-emit-import-library chicken.load
read-syntax.c: read-syntax.scm common-declarations.scm
	$(bootstrap-lib) -emit-import-library chicken.read-syntax
repl.c: repl.scm common-declarations.scm
	$(bootstrap-lib) -emit-import-library chicken.repl
expand.c: expand.scm synrules.scm common-declarations.scm
	$(bootstrap-lib) \
	-no-module-registration
modules.c: modules.scm common-declarations.scm mini-srfi-1.scm
	$(bootstrap-lib)
extras.c: extras.scm common-declarations.scm
	$(bootstrap-lib) \
	-emit-import-library chicken.format \
	-emit-import-library chicken.io \
	-emit-import-library chicken.pretty-print \
	-emit-import-library chicken.random
posixunix.c: posix.scm posixunix.scm posix-common.scm common-declarations.scm
	$(bootstrap-lib) -feature platform-unix \
	-emit-import-library chicken.errno \
	-emit-import-library chicken.file.posix \
	-emit-import-library chicken.time.posix \
	-emit-import-library chicken.process \
	-emit-import-library chicken.process.signal \
	-emit-import-library chicken.process-context.posix \
	-no-module-registration
irregex.c: irregex.scm irregex-core.scm irregex-utils.scm common-declarations.scm \
		chicken.fixnum.import.scm
	$(bootstrap-lib) -emit-import-library chicken.irregex
chicken-syntax.c: chicken-syntax.scm common-declarations.scm mini-srfi-1.scm \
		chicken.fixnum.import.scm
	$(bootstrap-lib)
chicken-ffi-syntax.c: chicken-ffi-syntax.scm common-declarations.scm mini-srfi-1.scm \
			chicken.format.import.scm
	$(bootstrap-lib)
continuation.c: continuation.scm common-declarations.scm \
		chicken.fixnum.import.scm
	$(bootstrap-lib) -emit-import-library chicken.continuation
data-structures.c: data-structures.scm common-declarations.scm
	$(bootstrap-lib) \
	-emit-import-library chicken.sort \
	-emit-import-library chicken.string
pathname.c: pathname.scm common-declarations.scm
	$(bootstrap-lib) -emit-import-library chicken.pathname
port.c: port.scm common-declarations.scm
	$(bootstrap-lib) -emit-import-library chicken.port
file.c: file.scm common-declarations.scm
	$(bootstrap-lib) -emit-import-library chicken.file
lolevel.c: lolevel.scm common-declarations.scm \
		chicken.base.import.scm chicken.fixnum.import.scm \
		chicken.foreign.import.scm
	$(bootstrap-lib) \
	-emit-import-library chicken.locative \
	-emit-import-library chicken.memory \
	-emit-import-library chicken.memory.representation
srfi-4.c: srfi-4.scm common-declarations.scm \
		chicken.base.import.scm chicken.bitwise.import.scm \
		chicken.fixnum.import.scm chicken.foreign.import.scm \
		chicken.gc.import.scm chicken.platform.import.scm \
		chicken.syntax.import.scm
	$(bootstrap-lib) -emit-import-library srfi-4
stub.c: stub.scm common-declarations.scm chicken.platform.import.scm
	$(bootstrap-lib)
build-version.c: build-version.scm buildversion buildbranch buildid
	$(bootstrap-lib)
eval-modules.c: eval-modules.scm common-declarations.scm \
		chicken.bitwise.import.scm chicken.blob.import.scm \
		chicken.foreign.import.scm chicken.fixnum.import.scm \
		chicken.keyword.import.scm chicken.internal.import.scm \
		chicken.platform.import.scm chicken.errno.import.scm \
		chicken.condition.import.scm chicken.string.import.scm \
		chicken.irregex.import.scm chicken.pathname.import.scm \
		chicken.io.import.scm chicken.port.import.scm \
		chicken.time.import.scm chicken.memory.import.scm \
		chicken.file.posix.import.scm chicken.flonum.import.scm \
		chicken.format.import.scm chicken.gc.import.scm \
		chicken.eval.import.scm chicken.load.import.scm \
		chicken.locative.import.scm chicken.memory.representation.import.scm \
		chicken.plist.import.scm chicken.pretty-print.import.scm \
		chicken.process.import.scm chicken.process.signal.import.scm \
		chicken.process-context.import.scm \
		chicken.process-context.posix.import.scm chicken.random.import.scm \
		chicken.sort.import.scm chicken.time.posix.import.scm \
		chicken.continuation.import.scm chicken.file.import.scm \
		chicken.read-syntax.import.scm chicken.repl.import.scm \
		srfi-4.import.scm
	$(bootstrap-lib)

.PHONY: clean
clean:
	$(RM) libchicken.a $(UNITS:%=%.o) $(filter-out runtime.c,$(wildcard *.c))
