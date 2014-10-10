TESTSCM = app begin call-cc-simple call-cc closure-1 display if lambda one-liners prim set simple-if 
# TODO: recursion square tail-call-optimization
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

test: $(TESTFILES)
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./run.sh tests/$(f).scm && rm -rf $(f);)

.PHONY: repl
repl:
	huski compiler.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.c *.out tags
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c;)
