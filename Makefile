PREFIX ?= /usr
AR ?= ar

# TODO: detect these better?
CSI:=csi-5.2
export CHICKEN:=chicken-5.2

export CC := gcc
export NEEDED_CFLAGS = -D_GNU_SOURCE -DHAVE_CHICKEN_CONFIG_H -DC_ENABLE_PTABLES \
		-fwrapv -fno-strict-aliasing -Wno-unused -fno-stack-protector
export CFLAGS ?= -ffunction-sections -fdata-sections -static-pie -O2

# TODO: set CHICKEN_FEATURES automagically?
export CHICKEN_FEATURES
export CHICKEN_FLAGS := -optimize-level 3 -disable-interrupts -clustering -setup-mode \
		-include-path libchicken/ -consult-types-file libchicken/types.db
export LDFLAGS ?= -Wl,-gc-sections

SLDS:=$(wildcard *.sld)
MODS:=$(SLDS:%.sld=%.mod.scm)
UNITS:=distill.fetch distill.hash distill.nproc \
	distill.plan distill.package distill.execline \
	distill.filepath distill.eprint distill.memo \
	distill.text distill.base distill.system \
	distill.image distill.unix distill.tai64 \
	distill.service distill.sysctl distill.fs \
	distill.net distill.kvector distill.contract \
	distill.coroutine srfi-69 matchable

.PHONY: test all install snapshot
all: distill ${UNITS:%=%.o}
VERSION ?= $(shell git rev-parse --short HEAD)

libchicken/libchicken.a:
	$(MAKE) -C libchicken/ libchicken.a

snapshot: distill-$(VERSION)-snapshot.tar.zst

distill-$(VERSION)-snapshot.tar.zst:
	git archive --format=tar --prefix distill-$(VERSION)/ HEAD | zstd - -o $@

Makefile.dep: $(wildcard *.sld) autodep.scm
	$(CSI) -s autodep.scm > Makefile.dep

include Makefile.dep

distill.plan.c: copy-sparse.c
distill.hash.c: blake2b-ref.c blake2.h blake2-impl.h

%.c %.import.scm: vendor/%.scm
	$(CHICKEN) $< -unit $* -static $(CHICKEN_FEATURES) $(CHICKEN_FLAGS) \
		-feature compiling-static-extension \
		-emit-all-import-libraries -module-registration \
		-output-file $*.c

distill.%.c distill.%.import.scm: %.mod.scm
	$(CHICKEN) $< -unit $* -static $(CHICKEN_FEATURES) $(CHICKEN_FLAGS) \
		-feature compiling-static-extension \
		-emit-all-import-libraries -module-registration \
		-emit-types-file distill.$*.types \
		-emit-link-file distill.$*.link \
		-output-file distill.$*.c

distill.c: distill.scm ${UNITS:%=%.import.scm}
	$(CHICKEN) $< -module main -static $(CHICKEN_FEATURES) $(CHICKEN_FLAGS)

%.o: %.c
	$(CC) $(NEEDED_CFLAGS) $(CFLAGS) -I./libchicken -c $^ -o $@

distill: distill.o ${UNITS:%=%.o} libchicken/libchicken.a
	$(CC) $(NEEDED_CFLAGS) $(CFLAGS) -I./libchicken -o $@ $^ $(LDFLAGS) libchicken/libchicken.a

TESTS:=$(wildcard *-test.scm)
test: distill $(TESTS)
	@for x in $(TESTS); do echo $$x; ./distill run $$x || exit 1; done

clean:
	$(MAKE) -C libchicken/ clean
	$(RM) distill Makefile.dep *.types *.import.scm *.mod.scm *.so *.o *.link ${UNITS:%=%.c}

install: all
	install -D -m 0775 -t $(DESTDIR)$(PREFIX)/bin distill
	install -d -m 0775 $(DESTDIR)$(PREFIX)/lib/distill
	cp -a pkg plat svc patches $(DESTDIR)$(PREFIX)/lib/distill
