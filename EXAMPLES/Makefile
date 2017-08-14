OCAMLC = ocamlc ../interval.cma -I ..
OCAMLOPT = ocamlopt ../interval.cmxa -I ..

all: norm opt

norm: example.norm precision.norm

opt: example.opt precision.opt


example.opt: example.ml
	$(OCAMLOPT) example.ml -o example.opt

example.norm: example.ml
	$(OCAMLC) example.ml -o example.norm

precision.opt: precision.ml
	$(OCAMLOPT) precision.ml -o precision.opt

precision.norm: precision.ml
	$(OCAMLC) precision.ml -o precision.norm

clean:
	$(RM) -f *.norm *.opt *.o *.cmx *.cmo *.cmi *~
