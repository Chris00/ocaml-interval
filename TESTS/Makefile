OCAMLDIR:=$(shell ocamlc -where)
OCAMLOPT = ocamlopt -I ..

tests: tests.ml
	$(OCAMLOPT) -o tests ../interval.cmxa unix.cmxa tests.ml


clean:
	$(RM) -f *.cmo *.cmi *.cmx *.o *~  tests

