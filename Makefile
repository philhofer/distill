
R7RSI:=csi
CSC_FLAGS:=-O3 -disable-interrupts -clustering
R7RSC:=csc $(CSC_FLAGS)
CSC_LIBFLAGS:=-regenerate-import-libraries -setup-mode -D compiling-extension \
	-D compiling-static-extension -static -J

SLDS=$(wildcard *.sld)
MODS=$(SLDS:%.sld=%.mod.scm)

Makefile.dep: $(wildcard *.sld) autodep.scm
	$(R7RSI) -s autodep.scm > Makefile.dep

include Makefile.dep

# these are the translation units that make up the final binary
UNITS:=distill.hash distill.nproc distill.table \
	distill.plan distill.package distill.execline \
	distill.filepath distill.eprint distill.memo \
	distill.sequence distill.buildenv distill.base \
	distill.image distill.unix distill.linux \
	distill.service distill.sysctl distill.fs \
	distill.net

%.import.scm %.o:
	$(R7RSC) $(CSC_LIBFLAGS) -unit $* -ot $*.types -c $< -o $*.o

distill: distill.scm ${UNITS:%=%.o} ${UNITS:%=%.import.scm}
	$(R7RSC) -setup-mode -m main -static -L -static-pie $< ${UNITS:%=%.o} -o $@

TESTS:=$(wildcard *-test.scm)
.PHONY: test all

test: distill $(TESTS)
	./sysplan $(TESTS)

all: distill ${UNITS:%=%.o}

clean:
	$(RM) distill Makefile.dep *.types *.import.scm *.mod.scm *.so *.o *.link
