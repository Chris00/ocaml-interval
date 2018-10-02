PACKAGES = $(basename $(wildcard *.opam))
PKGVERSION = $(shell git describe --always)

all build:
	dune build @install @examples
	dune build @runtest --force

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
	else echo "All tests passed successfully"; fi

graphs:
	dune build @graphs
	@echo See $(wildcard _build/default/tests/*.pdf)

clean:
	dune clean

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/src-intel/interval_intel.mli
	dune build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

# until we have https://github.com/ocaml/opam-publish/issues/38
bistro: lint
	topkg distrib
	topkg publish distrib
# 	Create packages and perform the subtitution that topkg does not
#	(until opam2, https://discuss.ocaml.org/t/sync-versions-of-several-packages-coming-from-a-single-repo/808/5)
	for p in $(PACKAGES); do \
	  topkg opam pkg -n $$p; \
	  sed -e 's/\(^ *"interval_[a-z]\+"\) */\1 {= "$(PKGVERSION)"}/' \
	    --in-place _build/$$p.$(PKGVERSION)/opam; \
	done
	@[ -d packages ] || (echo \
	  "ERROR: Make a symbolic link packages → opam-repo/packages"; exit 1)
	for p in $(PACKAGES); do \
	  mkdir -p packages/$$p; \
	  cp -r _build/$$p.$(PKGVERSION) packages/$$p/; \
	done
	cd packages && git add $(PACKAGES)
#	CONDUIT_TLS=native topkg opam submit $(addprefix -n, $(PACKAGES))

pin:
#	Installation in the right order
	opam pin -k path -y add interval_base .
	opam pin -k path -y add interval_intel .
	opam pin -k path -y add interval_crlibm .

unpin:
	opam pin remove -y $(PACKAGES)

upgrade:
	opam upgrade -y $(PACKAGES)

lint:
	@for p in $(PACKAGES); do opam lint $$p.opam; done

.PHONY: all build ocamlfpu install uninstall tests graphs \
  examples clean doc bistro pin unpin upgrade lint
