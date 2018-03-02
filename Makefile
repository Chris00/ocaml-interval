PKGVERSION = $(shell git describe --always --dirty)

all build byte native:
	jbuilder build @install #--dev
	jbuilder build @examples
	jbuilder build @runtest

ocamlfpu: all
	cd _build/default/src/ && ocamlmktop -I . -o ocamlfpu interval.cma

install uninstall:
	jbuilder $@

tests: all
	cd _build/default/TESTS/ && ./tests.exe

clean:
	jbuilder clean

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/interval.mli \
	  > _build/default/src/interval.mli
	jbuilder build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

lint:
	opam lint interval.opam

.PHONY: all build byte native ocamlfpu install uninstall tests \
  examples EXAMPLES doc lint
