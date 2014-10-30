TESTSCM = adder app begin call-cc-set call-cc-simple call-cc closure-1 closure-sub display fac if lambda one-liners prim quote recursion scoping set simple-if square
# Cannot auto test tail-call-optimization because it runs forever
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

cyclone: cyclone.scm trans.scm cgen.scm
	csc cyclone.scm

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
	rm -rf a.out *.o *.c *.out tags cyclone
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
