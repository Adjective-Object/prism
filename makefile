all: prism
.PHONY: clean

#TEST_IMG=images/dock_tiny.jpg
TEST_IMG=images/road.jpg
BUILD_PATH=dist/build

prism: src/*
	cabal install --only-dependencies
	cabal configure
	cabal build
	cp $(BUILD_PATH)/prism/prism ./prism

prism-prof: src/*
	cabal install --only-dependencies
	cabal configure \
		--enable-executable-profiling \
		--enable-benchmarks \
		--enable-tests
	cabal build
	cp $(BUILD_PATH)/prism/prism ./prism-prof

test: prism 
	time sh -c 'cat $(TEST_IMG) | ./prism -'

proftest: prism-prof
	time sh -c 'cat $(TEST_IMG)| ./prism-prof - +RTS -p -RTS'

clean:
	rm -f *.o
	rm -f *.hi
	rm -f prism
	rm -f prism-prof
