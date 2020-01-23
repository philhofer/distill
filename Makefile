
R7RSI:=csi
CSC_FLAGS:=-O3 -disable-interrupts -d1 -clustering
R7RSC:=csc $(CSC_FLAGS)
CSC_LIBFLAGS:=-regenerate-import-libraries -setup-mode -D compiling-extension \
	-D compiling-static-extension -static -J

SLDS=$(wildcard *.sld)
MODS=$(SLDS:%.sld=%.mod.scm)

Makefile.dep $(MODS): $(wildcard *.sld) autodep.scm
	$(R7RSI) -s autodep.scm > Makefile.dep

include Makefile.dep

# these are the translation units that make up the final binary
UNITS:=hash table plan package execline filepath eprint memo base

%.import.scm %.o:
	$(R7RSC) $(CSC_LIBFLAGS) -unit $* -ot $*.types -c $< -o $*.o

sysplan: sysplan.scm ${UNITS:%=%.o} ${UNITS:%=%.import.scm}
	$(R7RSC) -setup-mode -m main -static -L -static-pie $< ${UNITS:%=%.o} -o $@

TESTS:=$(wildcard *-test.scm)
.PHONY: test all

test: sysplan $(TESTS)
	./sysplan $(TESTS)

all: sysplan ${UNITS:%=%.o}

clean:
	$(RM) sysplan *.types *.import.scm *.mod.scm *.so *.o *.link
