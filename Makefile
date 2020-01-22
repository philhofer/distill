
R7RSI:=csi -R r7rs -s
CSC_FLAGS:=-O3
R7RSC:=csc -X r7rs -R r7rs $(CSC_FLAGS)
CSC_LIBFLAGS:=-regenerate-import-libraries -setup-mode -D compiling-extension \
	-D compiling-static-extension -compile-syntax -static -J

Makefile.dep: $(wildcard *.sld) autodep.scm
	$(R7RSI) autodep.scm > $@

include Makefile.dep

UNITS:=hash table plan execline filepath log memo base

%.import.scm %.types %.o:
	$(R7RSC) $(CSC_LIBFLAGS) -unit $* -ot $*.types -c $<

sysplan: sysplan.scm ${UNITS:%=%.o} ${UNITS:%=%.import.scm}
	$(R7RSC) -setup-mode -m main -static -L -static-pie $< ${UNITS:%=%.o} -o $@

TESTS:=$(wildcard *-test.scm)
.PHONY: test all

test: sysplan $(TESTS)
	./sysplan $(TESTS)

all: sysplan ${UNITS:%=%.o}

clean:
	$(RM) sysplan *.types *.import.scm *.so *.o *.link
