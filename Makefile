OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep

OBJS=def.cmo const.cmo print.cmo printtoC.cmo tools.cmo trees.cmo algos.cmo algosDiscret.cmo brute.cmo script.cmo
OPTOBJS=def.cmx const.cmx print.cmx printtoC.cmx tools.cmx trees.cmx algos.cmx algosDiscret.cmx brute.cmx script.cmx

CMOBJS=def.cmo const.cmo print.cmo printtoC.cmo tools.cmo trees.cmo algos.cmo algosDiscret.cmo brute.cmo script.cmo
CMOPTOBJS=def.cmx const.cmx print.cmx printtoC.cmx tools.cmx trees.cmx algos.cmx algosDiscret.cmx brute.cmx script.cmx

all : dep comp

opt: dep optcomb

comp : $(OBJS)
	$(OCAMLC) -o comp unix.cma $(CMOBJS)

optcomb: $(OPTOBJS)
	$(OCAMLOPT) -o comp unix.cmxa $(CMOPTOBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

.mly.ml:
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<
.mll.ml:
	$(OCAMLLEX) $<

dep:
	$(OCAMLDEP) *.ml *.mli > .dep

clean:
	rm -f *.o *.cmx *.cmo *.cmi *.out *.pdf *.toc *.blg *.bbl *.lot

include .dep
