TESTSCM = adder app apply begin call-cc-set call-cc-simple call-cc closure-1 closure-sub display fac if lambda one-liners prim quote recursion scoping set simple-if square strings unit-tests
# Cannot auto test tail-call-optimization because it runs forever
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

all: cyclone

trans.so: trans.scm
	csc -s trans.scm

cgen.so: cgen.scm
	csc -s cgen.scm

parser.so: parser.scm
	csc -s parser.scm

cyclone: cyclone.scm trans.so cgen.so parser.so
	csc cyclone.scm

.PHONY: test
test: $(TESTFILES) cyclone
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./cyclone tests/$(f).scm && tests/$(f) && rm -rf tests/$(f);)

# TODO: some kind of repl, but requires eval
#.PHONY: repl
#repl:
#	huski cyclone.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.c *.out tags cyclone
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
