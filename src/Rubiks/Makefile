OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
BATTERIES = `ocamlfind query batteries`
LIBRARIES =  ../lib/lib1.cma 
XLIBRARIES =  unix.cmxa nums.cmxa batteries.cmxa batteries_unthreaded.cmxa ../lib/lib1.cmxa

LIB = ../lib
LIB_INCLUDES = -I $(LIB)/Utilities -I $(LIB)/Problem -I $(LIB)/Solver/Tree -I $(LIB)/Solver/IteratedDeepening -I $(LIB)/Solver/IteratedDeepeningTree 
LOCAL_INCLUDES = -I Examples  
INCLUDES=  -I +unix -I $(BATTERIES) -I $(BATTERIES)/unthreaded 
OCAMLFLAGS=$(INCLUDES) $(LIB_INCLUDES) $(LOCAL_INCLUDES)    


# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
PROG1_OBJS= Examples/Rubiks.cmo RubiksTest.cmo
PROG1_XOBJS= Examples/Rubiks.cmx RubiksTest.cmx

prog1: $(PROG1_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $(LIBRARIES) $(PROG1_OBJS)
prog1.opt: $(PROG1_XOBJS)
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) $(XLIBRARIES) $(PROG1_XOBJS)


# Common rules

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	rm -f prog1 
	rm -f *.cm[iox]
	rm -f */*.cmo
	rm -f */*.cmi

# Test
test: prog1
	./prog1
xtest: prog1.opt
	./prog1.opt

# Dependencies
depend:
	$(OCAMLDEP) $(LOCAL_INCLUDES) Example/*.mli Example/*.mli *.mli *.ml > .depend

include .depend

# Documentation
doc:
	ocamldoc -d html -charset utf8 -html $(OCAMLFLAGS) \
	Examples/*.ml 

