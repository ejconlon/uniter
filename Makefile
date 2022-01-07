include Makefile.base

.PHONY: example-gen
example-gen:
	stack run uniter-example

.PHONY: example-dotall
example-dotall:
	./dot/dotall.sh

.PHONY: example
example: example-gen example-dotall
