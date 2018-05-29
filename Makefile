PKGVERSION = $(shell git describe --always --dirty)

all build byte native:
	jbuilder build @install @examples --dev
	jbuilder build @runtest --force

ocamlfpu: all
	cd _build/default/src/ && ocamlmktop -I . -o ocamlfpu interval.cma

install uninstall:
	jbuilder $@

tests: all
	@ echo "Run tests..."
	@ cd _build/default/tests/ && ./tests.exe > tests.log && \
	C=`grep DEFINED tests.log | wc -l` && \
	if [ $$C -gt 0 ]; then \
	  echo "Tests: $$C errors (see _build/default/tests/tests.log)"; \
	else echo "All tests passed successfully"; fi

clean:
	jbuilder clean

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/interval.mli \
	  > _build/default/src/interval.mli
	jbuilder build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

lint:
	@opam lint interval_base.opam
	@opam lint interval_intel.opam
	@opam lint interval_crlibm.opam

.PHONY: all build byte native ocamlfpu install uninstall tests \
  examples doc lint
