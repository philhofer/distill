
R7RSI:=csi -R r7rs -s
CSC_FLAGS:=-O3
R7RSC:=csc -X r7rs -R r7rs $(CSC_FLAGS)

Makefile.dep: $(wildcard *.sld) autodep.scm
	$(R7RSI) autodep.scm > $@

include Makefile.dep

UNITS:=hash table plan execline filepath log

%.import.scm %.types %.o:
	$(R7RSC) -setup-mode -D compiling-extension -D compiling-static-extension -unit $* -static -J -ot $*.types -c $<

%.import.so: %.import.scm
	$(R7RSC) -O3 -s $<

sysplan: sysplan.scm ${UNITS:%=%.o} ${UNITS:%=%.import.so}
	csc -X r7rs -R r7rs -setup-mode -m main -O3 -static $< -o $@

TESTS:=$(wildcard *-test.scm)
.PHONY: test all

test: sysplan $(TESTS)
	./sysplan $(TESTS)

all: sysplan ${UNITS:%=%.o} ${UNITS:%=%.import.so}

bootstrap.tar.xz:
	./mkbootstrap.sh

clean:
	$(RM) sysplan *.types *.import.scm *.so *.o *.link
