all: prism
.PHONY: clean prism

TEST_IMG="images/dock_tiny.jpg"
BUILD_PATH=dist/build

prism: src
	cabal configure
	cabal build
	cp $(BUILD_PATH)/prism/prism ./prism

prism-prof: src
	cabal configure \
		--enable-library-profiling \
		--enable-executable-profiling \
		--enable-tests \
		--enable-benchmarks
	cabal build
	cp $(BUILD_PATH)/prism/prism ./prism-prof

test: prism
	./prism +RTS -p -RTS

proftest: prism-prof
	.prism +RTS -p -RTS


prof-vector:
	ghc -prof -fprof-cafs -fprof-auto-calls src/prism-vector.hs
	cat $(TEST_IMG) | ./prism +RTS -p -RTS 

clean:
	rm -f *.o
	rm -f *.hi
	rm -f prism
	rm -f prism-vector
