include Makefile.base

.PHONY: example-gen
example-gen:
	stack build --test --no-run-tests --exec uniter-example

.PHONY: example-dotall
example-dotall:
	./dot/dotall.sh

.PHONY: example
example: example-gen example-dotall
