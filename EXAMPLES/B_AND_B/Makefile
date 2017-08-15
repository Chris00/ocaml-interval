INCLUDES= -I ../..
OCAMLC_FLAGS=$(INCLUDES)
OCAMLOPT_FLAGS=$(INCLUDES)

OCAMLC = ocamlc $(OCAMLC_FLAGS)
OCAMLOPT = ocamlopt $(OCAMLOPT_FLAGS)
OCAMLDEP = ocamldep

TESTNORM_OBJS= pqueue.cmo b_and_b.cmo main.cmo
TESTOPT_OBJS=  $(TESTNORM_OBJS:.cmo=.cmx)

all: test.norm test.opt

test.opt : $(TESTOPT_OBJS)
	$(OCAMLOPT) -o $@ ../../interval.cmxa $(TESTOPT_OBJS)

test.norm : $(TESTNORM_OBJS)
	$(OCAMLC)  -o $@ ../../interval.cma $(TESTNORM_OBJS)


.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo :
	$(OCAMLC) -c $<
.mli.cmi :
	$(OCAMLC) -c $<
.ml.cmx :
	$(OCAMLOPT) -c $<

clean:
	$(RM) -f *.cmo *.cmi *.cmx *.o *.a *.cma *.cmxa test.norm test.opt *~

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend

