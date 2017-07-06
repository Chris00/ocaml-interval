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

install: all
	ocamlfind install interval $(wildcard META \
	  *.cmi *.cmti *.cma *.cmx *.cmxa *.mli *.a *.so)

remove:
	ocamlfind remove interval

.PHONY: install remove

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

doc/index.html: *.mli
	ocamldoc -d doc -html *.mli

depend:
	ocamldep *.mli *.ml > .depend

include .depend
