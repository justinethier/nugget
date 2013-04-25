all: config install
config:
	cabal configure --prefix=${HOME}
build:
	cabal build
install:
	cabal install

nsc: nugget.hs
	ghc nugget.hs -o nsc

.PHONY: clean
clean:
	rm -rf *.o *.c *.hi nsc nugget a.out 90-min-scc/*.c tests/*.c tests/a.out dist
