TESTSCM = adder app begin call-cc-set call-cc-simple call-cc closure-1 closure-sub display if lambda one-liners prim quote set simple-if square
# TODO: fac recursion 
# Cannot auto test tail-call-optimization because it runs forever
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

compiler: compiler.scm trans.scm cgen.scm
	csc compiler.scm

test: $(TESTFILES) compiler
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./cyc tests/$(f).scm && rm -rf $(f);)

.PHONY: repl
repl:
	huski compiler.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.c *.out tags compiler
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c;)
