all: main test

TEST_IMG="stand_tiny.jpg"

main: 
	ghc prism.hs

test:
	cat $(TEST_IMG) | ./prism
