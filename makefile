all: prof

TEST_IMG="dock_tiny.jpg"

prism: prism.hs
	ghc -p prism.hs

prism-vector: prism-vector.hs
	ghc prism-vector.hs

.PHONY: prof
prof:
	ghc -prof -fprof-cafs -fprof-auto-calls prism.hs
	cat $(TEST_IMG) | ./prism +RTS -p -RTS

test-vector:
	cat $(TEST_IMG) | ./prism-vector
