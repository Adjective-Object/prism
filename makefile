all: main test

TEST_IMG="dock_tiny.jpg"

main:	
	ghc prism.hs

test:
	cat $(TEST_IMG) | ./prism
