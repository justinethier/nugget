TESTSCM = app begin call-cc-simple call-cc closure-1 display if lambda one-liners prim set simple-if 
# TODO: recursion square tail-call-optimization
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
