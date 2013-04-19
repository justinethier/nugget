nugget: nugget.hs
	ghc nugget.hs

.PHONY: clean
clean:
	rm -rf *.o *.c *.hi nugget a.out tests/*.c tests/a.out
