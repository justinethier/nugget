all: config install
config:
	cabal configure --prefix=${HOME}
build:
	cabal build
install:
	cabal install

# Use of the Paths_ module from cabal breaks this directive
nsc: nugget.hs
	ghc nugget.hs -o nsc

.PHONY: clean
clean:
	rm -rf *.o *.c *.hi nsc 90-min-scc/*.c tests/*.c dist
	# Remove all compiled executables
	find 90-min-scc -type f \( ! -iname "*.scm" \) -perm +111 -delete
	find tests -type f -perm +111 -delete
