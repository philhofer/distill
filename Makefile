
R7RSI:=csi -R r7rs -s
CSC_FLAGS:=-O3 -explicit-use
R7RSC:=csc -X r7rs -R r7rs $(CSC_FLAGS) -s -J

Makefile.dep: $(wildcard *.sld)
	$(R7RSI) autodep.scm > $@

include Makefile.dep

%.import.scm %.types %.so:
	$(R7RSC) -ot $*.types $<

TESTS:=$(wildcard *-test.scm)
.PHONY: test all

test: $(TESTS) ${TESTS:%-test.scm=%.so}
	@for x in $(TESTS); do $(R7RSI) $$x; echo $$x OK; done
	@for x in "*-test.sh"; do ./$$x; echo $$x OK; done

all: hash.so plan.so execline.so filepath.so log.so

bootstrap.tar.xz:
	./mkbootstrap.sh

clean:
	$(RM) *.import.scm *.so *.o
