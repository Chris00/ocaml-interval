PACKAGES = $(basename $(wildcard *.opam))
PKGVERSION = $(shell git describe --always)

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
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/src/interval.mli
	jbuilder build @doc
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

lint:
	@opam lint interval.opam
	@opam lint interval_base.opam
	@opam lint interval_intel.opam
	@opam lint interval_crlibm.opam

.PHONY: all build byte native ocamlfpu install uninstall tests \
  examples doc bistro lint
