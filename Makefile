
# NOTE: you need at least chicken-5.2 to build this code;
# earlier chickens do not know about the -M flag
PREFIX ?= /usr

CSI:=csi-5.2
CSC_FLAGS:=-C -D_GNU_SOURCE -O3 -disable-interrupts -clustering
CSC:=csc-5.2
CSC_LIBFLAGS:=-setup-mode -D compiling-static-extension -static -J -M
CHICKEN_DO:=chicken-do-5.2

SLDS:=$(wildcard *.sld)
MODS:=$(SLDS:%.sld=%.mod.scm)
UNITS:=distill.hash distill.nproc \
	distill.plan distill.package distill.execline \
	distill.filepath distill.eprint distill.memo \
	distill.text distill.base distill.system \
	distill.image distill.unix distill.tai64 \
	distill.service distill.sysctl distill.fs \
	distill.net distill.kvector distill.contract


.PHONY: test all tools install
all: tools distill ${UNITS:%=%.o}

Makefile.dep: $(wildcard *.sld) autodep.scm
	$(CSI) -s autodep.scm > Makefile.dep

include Makefile.dep

distill.plan.o: copy-sparse.c

# since chicken won't touch %.import.scm
# unless a module's exports (or syntax) have changed,
# we can avoid unnecessary recompilation
# HOWEVER, Make doesn't like seeing %.import.scm older than %.o,
# which means this rule may match spuriously during incremental
# compiles, so we use chicken-do to avoid actually doing real work
%.import.scm %.o:
	@echo "csc $*"
	@$(CHICKEN_DO) $*.o : $^ : $(CSC) $(CSC_FLAGS) $(CSC_LIBFLAGS) -unit $* -ot $*.types -c $< -o $*.o

distill: distill.scm ${UNITS:%=%.o}
	@echo "link $@"
	@$(CSC) $(CSC_FLAGS) -setup-mode ${UNITS:%=-uses %} -m main -static -L -static-pie $< ${UNITS:%=%.o} -o $@

tools:
	+$(MAKE) -C tools/

TESTS:=$(wildcard *-test.scm)
test: distill $(TESTS)
	@for x in $(TESTS); do echo $$x; ./distill run $$x || exit 1; done

clean:
	$(RM) distill Makefile.dep *.types *.import.scm *.mod.scm *.so *.o *.link

install: all
	install -D -m 0775 -t $(DESTDIR)$(PREFIX)/bin distill
	install -d -m 0775 $(DESTDIR)$(PREFIX)/lib/distill
	cp -a pkg plat svc patches $(DESTDIR)$(PREFIX)/lib/distill
