# NOTE: you need at least chicken-5.2 to build this code;
# earlier chickens do not know about the -M flag
PREFIX ?= /usr

# TODO: detect these better?
CHICKEN_INSTALL:=chicken-install-5.2
CSI:=csi-5.2
CHICKEN:=chicken-5.2

CC:=gcc
# NOTE: you *really* do not want to remove "-fwrapv -fno-strict-aliasing"
CFLAGS:=-ffunction-sections -fdata-sections -D_GNU_SOURCE -DHAVE_CHICKEN_CONFIG_H -DC_ENABLE_PTABLES \
	-I/usr/include/chicken-5.2/ -static-pie -O2 -fwrapv -fno-strict-aliasing -Wno-unused
CHICKEN_FLAGS:=-optimize-level 3 -disable-interrupts -clustering -setup-mode
CHICKEN_LIBDIR=$(shell \$(CHICKEN_INSTALL) -repository)
# TODO: vendor these dependencies
LDFLAGS:=-Wl,-gc-sections -lchicken-5.2

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

.PHONY: test all install
all: distill ${UNITS:%=%.o}

Makefile.dep: $(wildcard *.sld) autodep.scm
	$(CSI) -s autodep.scm > Makefile.dep

include Makefile.dep

distill.plan.c: copy-sparse.c

%.c %.import.scm: vendor/%.scm
	$(CHICKEN) $< -unit $* -static $(CHICKEN_FLAGS) \
		-feature compiling-static-extension \
		-emit-all-import-libraries -module-registration \
		-output-file $*.c

distill.%.c distill.%.import.scm: %.mod.scm
	$(CHICKEN) $< -unit $* -static $(CHICKEN_FLAGS) \
		-feature compiling-static-extension \
		-emit-all-import-libraries -module-registration \
		-emit-types-file distill.$*.types \
		-emit-link-file distill.$*.link \
		-output-file distill.$*.c

distill.c: distill.scm ${UNITS:%=%.import.scm}
	$(CHICKEN) $< -module main -static $(CHICKEN_FLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

distill: distill.o ${UNITS:%=%.o}
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

TESTS:=$(wildcard *-test.scm)
test: distill $(TESTS)
	@for x in $(TESTS); do echo $$x; ./distill run $$x || exit 1; done

clean:
	$(RM) distill Makefile.dep *.types *.import.scm *.mod.scm *.so *.o *.link ${UNITS:%=%.c}

install: all
	install -D -m 0775 -t $(DESTDIR)$(PREFIX)/bin distill
	install -d -m 0775 $(DESTDIR)$(PREFIX)/lib/distill
	cp -a pkg plat svc patches $(DESTDIR)$(PREFIX)/lib/distill
