all: prism prism-vector test

TEST_IMG="dock_tiny.jpg"

prism:	
	ghc prism.hs

prism-vector:	
	ghc prism-vector.hs

test:
	cat $(TEST_IMG) | ./prism

test-vector:
	cat $(TEST_IMG) | ./prism-vector
