OCAMLDIR:= "$(shell ocamlc -where)"
CSLC = ocamlc -annot -bin-annot
CSLOPT = ocamlopt -annot -bin-annot
NORM_OBJS= fpu.cmo fpu_rename_all.cmo fpu_rename.cmo interval.cmo 
OPT_OBJS=  $(NORM_OBJS:.cmo=.cmx)
CC = gcc

all : interval.cma interval.cmxa doc/index.html ocamlfpu


interval.cma interval.cmxa: $(NORM_OBJS) $(OPT_OBJS) chcw.o
	ocamlmklib -o interval -oc interval_ocaml $^

ocamlfpu: interval.cma
	ocamlmktop -I . -o ocamlfpu interval.cma

examples EXAMPLES:
	$(MAKE) -C EXAMPLES

install: all
	ocamlfind install interval $(wildcard META \
	  *.cmi *.cmti *.cma *.cmx *.cmxa *.mli *.a *.so)

remove:
	ocamlfind remove interval

tests:
	$(MAKE) -C TESTS

.PHONY: examples EXAMPLES install remove tests

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo :
	$(CSLC) -c $<
.mli.cmi :
	$(CSLC) -c $<
.ml.cmx :
	$(CSLOPT) -c $<
.c.o :
	$(CC) -W -Wall -O3 -I $(OCAMLDIR)/caml -fPIC -c  $<

clean:
	$(RM) -f *.cmo *.cmi *.cmx *.o *~ *.cma *.cmxa *.a a.out *.so ocamlfpu ocamlfpu.exe
	$(MAKE) -C TESTS $@
	$(MAKE) -C EXAMPLES $@
	$(MAKE) -C EXAMPLES/B_AND_B $@

.PHONY: doc
doc: doc/index.html

doc/index.html: $(wildcard *.mli)
	mkdir -p doc
	ocamldoc -d doc -html -charset utf-8 $^

depend:
	ocamldep *.mli *.ml > .depend

include .depend
