# ocamlc -pp 'camlp4o /usr/local/lib/ocaml/3.11.2/deriving/pa_deriving.cma' -I `ocamlfind query core` -I `ocamlfind query core_extended` -I `ocamlfind query deriving` -i ast.ml > ast.mli
# this code for creating interface
#
#
#
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
PP=-pp 'camlp4o /usr/local/lib/ocaml/3.11.2/deriving/pa_deriving.cma' 

#includes
INC_DER=-I `ocamlfind query deriving`
INC_JS= -I `ocamlfind query core` -I `ocamlfind query core_extended` 
INC_EXTLIB=-I ../ocaml/
NEKO_SRC_PATH=../neko/libs/include/ocaml/
 
INC_NEKO=-I $(NEKO_SRC_PATH)
INC_SWFLIB=-I ../ocaml/swflib/
INC_XML= -I ../ocaml/xml-light/ 
INCLUDES= $(INC_SWFLIB) $(INC_XML) $(INC_NEKO) -I ../ocaml/extc \
	  $(INC_EXTLIB) $(INC_JS) $(INC_DER) 
JS_LIB_PATH=-ccopt -L`ocamlfind query core` -ccopt -L`ocamlfind query core_extended`
LIBS_SHARED=extLib.cma extc.cma swflib.cma xml-light.cma unix.cma str.cma core.cma core_extended.cma deriving.cma  
OCAMLFLAGS=-g 
OCAMLOPTFLAGS=
OUTFILE=triss.main

HAXE_OBJS_BYTE=der_pmap.cmo ast.cmo lexer.cmo type.cmo type_comap.cmo common.cmo parser.cmo typecore.cmo genxml.cmo typeload.cmo codegen.cmo optimizer.cmo $(NEKO_SRC_PATH)nast.cmo $(NEKO_SRC_PATH)binast.cmo $(NEKO_SRC_PATH)nxml.cmo genneko.cmo genas3.cmo genjs.cmo genswf8.cmo genswf9.cmo genswf.cmo genphp.cmo gencpp.cmo interp.cmo typer.cmo 
HAXE_TARGETS_BYTE=der_pmap.cmo ast.cmi ast.cmo lexer.cmo type.cmo type_comap.cmo common.cmi common.cmo parser.cmo typecore.cmo genxml.cmo typeload.cmo codegen.cmo optimizer.cmo $(NEKO_SRC_PATH)nast.cmo $(NEKO_SRC_PATH)binast.cmo $(NEKO_SRC_PATH)nxml.cmo genneko.cmo genas3.cmo genjs.cmo genswf8.cmo genswf9.cmo genswf.cmo genphp.cmo gencpp.cmo interp.cmo typer.cmo 
TRISS_OBJS_BYTE=triss_int.cmo triss_main.cmo
TRISS_TARGETS= triss_int.cmo triss_main.cmo 
PROG1_OBJS=$(HAXE_OBJS_BYTE) $(TRISS_OBJS_BYTE)

all:   prog


depend:
		$(OCAMLDEP) $(PP) *.ml  > .depend

prog:  $(HAXE_TARGETS_BYTE) $(TRISS_OBJS_BYTE)
		$(OCAMLC) $(OCAMLFLAGS) -package bigarray,oUnit,type-conv,threads,pcre,sexplib,bin_prot,res,fieldslib -linkpkg -thread -custom $(INCLUDES) $(JS_LIB_PATH) $(LIBS_SHARED) -o   $(OUTFILE) $(PROG1_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mly.cmo:
		$(OCAMLC) $(PP) $(OCAMLFLAGS) $(INCLUDES) -c $<

.ml.cmo:
		$(OCAMLC) $(PP) $(OCAMLFLAGS) $(INCLUDES) $(LIBS_SHARED) -c $<

.mli.cmi:
		$(OCAMLC) $(PP) $(OCAMLFLAGS) $(INCLUDES) -c $<

celan: clean
clean:
		rm -f *.cmi *.cmo *.cmx *.o $(OUT) 

include .depend

