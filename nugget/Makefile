TESTSCM = app begin call-cc if lambda prim recursion set square
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

test: $(TESTFILES)
	$(foreach f,$(TESTSCM), echo tests/$(f) ; huski compiler.scm < tests/$(f).scm > $(f).c && gcc $(f).c -o $(f) && ./$(f);)

.PHONY: repl
repl:
	huski compiler.scm

.PHONY: clean
clean:
	rm -rf a.out *.o out.c tmp.c
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c;)
