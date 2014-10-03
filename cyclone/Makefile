TESTSCM = closure-1 one-liners set simple-if #app begin call-cc if lambda prim recursion square
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

test: $(TESTFILES)
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./run.sh tests/$(f).scm && rm -rf $(f);)
#	$(foreach f,$(TESTSCM), echo tests/$(f) ; huski compiler.scm < tests/$(f).scm > $(f).c && gcc $(f).c -o $(f) && ./$(f);)

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
