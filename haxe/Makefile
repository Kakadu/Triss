OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES= -I ../ocaml -I ../ocaml/swflib -I ../ocaml/xml-light -I ../ocaml/extc -I ../neko/libs/include/ocaml
SHAREDLIBS=extLib.cma extc.cma swflib.cma xml-light.cma unix.cma str.cma 
OCAMLFLAGS=-g -pp camlp4o
OCAMLOPTFLAGS=
OUTFILE=triss.main
NEKOINC=../neko/libs/include/ocaml/

HAXE_OBJS_BYTE=ast.cmo lexer.cmo type.cmo common.cmo parser.cmo typecore.cmo genxml.cmo typeload.cmo codegen.cmo optimizer.cmo $(NEKOINC)nast.cmo $(NEKOINC)binast.cmo $(NEKOINC)nxml.cmo genneko.cmo genas3.cmo genjs.cmo genswf8.cmo genswf9.cmo genswf.cmo genphp.cmo gencpp.cmo interp.cmo typer.cmo 
TRISS_OBJS_BYTE=triss_int.cmo
TRISS_TARGETS= triss_int.cmo 
PROG1_OBJS=$(HAXE_OBJS_BYTE) $(TRISS_OBJS_BYTE)

all:   prog

lexer:
		ocamllex lexer.mll

depend:
		ocamldep -pp camlp4o   *.ml  > .depend

prog:  $(HAXE_OBJS_BYTE) $(TRISS_OBJS_BYTE)
		$(OCAMLC) -custom -g -o $(OUTFILE) $(INCLUDES) $(SHAREDLIBS) $(PROG1_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mly.cmo:
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c $<

.ml.cmo:
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c $<

.mli.cmi:
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c $<

.mll.cmo:
		ocamlex $<

# Clean up
clean:
		rm -f *.cmi *.cmo *.cmx *.o 

include .depend
#asdf
