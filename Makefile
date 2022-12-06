PACKAGES = $(basename $(wildcard *.opam))
PKGVERSION = $(shell git describe --always)

all build:
	dune build --workspace dune-workspace.dev @install @examples
	dune build --workspace dune-workspace.dev @runtest --force

ocamlfpu: all
	cd _build/default/src/ && ocamlmktop -I . -o ocamlfpu interval.cma

install uninstall:
	dune $@

tests: all
	@ echo "Run tests..."
	@ cd _build/default/tests/ && ./tests.exe > tests.log && \
	C=`grep DEFINED tests.log | wc -l` && \
	if [ $$C -gt 0 ]; then \
	  echo "Tests: $$C errors (see _build/default/tests/tests.log)"; \
	  grep -B 3 -A 2 DEFINED tests.log; \
	else echo "All tests passed successfully."; fi

test-speed:
	dune exec tests/tests_speed.exe

graphs:
	dune build @graphs
	@echo See $(wildcard _build/default/tests/*.pdf)

clean:
	dune clean

doc:
	dune build @doc
	find _build/ -name index.html \
	  -exec sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place {} \;

pin:
#	Installation in the right order
	opam pin -k path -y add interval_base.dev .
	opam pin -k path -y add interval_intel.dev .
	opam pin -k path -y add interval_crlibm.dev .
	opam pin -k path -y add interval.dev .

unpin:
	opam pin remove -y $(PACKAGES)

upgrade:
	opam upgrade -y $(PACKAGES)

lint:
	opam lint

.PHONY: all build ocamlfpu install uninstall tests test-speed graphs \
  examples clean doc bistro pin unpin upgrade lint
