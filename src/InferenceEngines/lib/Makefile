OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
BATTERIES = `ocamlfind query batteries`
NUM = `ocamlfind query num`
BATLIB371 = batteries.cma  batteries_unthreaded.cma 
BATLIB = batteries.cma 

XBATLIB371 = batteries.cmxa  batteries_unthreaded.cmxa 
XBATLIB = batteries.cmxa 
 
# Ci-dessous, si le fichier batteries_unthreaded.cma est introuvable, remplacer BATLIB371 par BATLIB
LIBRARIES = unix.cma nums.cma $(BATLIB371) 
XLIBRARIES = unix.cmxa nums.cmxa $(XBATLIB371) 

LOCAL_INCLUDES = -I Utilities -I Problem -I Solver/Tree -I Solver/IteratedDeepening -I Solver/IteratedDeepeningTree
INCLUDES=  -I +unix -I $(NUM) -I $(BATTERIES) -I $(BATTERIES)/unthreaded 
OCAMLFLAGS=$(INCLUDES) $(LOCAL_INCLUDES)    

# The list of object files for prog1
LIB1_OBJS= Utilities/Additive.cmo\
	 Utilities/Pairs.cmo\
	 Utilities/Ord.cmo\
	 Utilities/Miscellaneous.cmo\
	Problem/StateProblem.cmo\
	Solver/Tree/Naive.cmo\
	Solver/Tree/DepthLimit.cmo\
	Solver/IteratedDeepening/Meta.cmo\
	Solver/IteratedDeepeningTree/IDS.cmo

LIB1_XOBJS= Utilities/Additive.cmx\
	 Utilities/Pairs.cmx\
	 Utilities/Ord.cmx\
	 Utilities/Miscellaneous.cmx\
	Problem/StateProblem.cmx\
	Solver/Tree/Naive.cmx\
	Solver/Tree/DepthLimit.cmx\
	Solver/IteratedDeepening/Meta.cmx\
	Solver/IteratedDeepeningTree/IDS.cmx

lib1.cma: $(LIB1_OBJS)
	$(OCAMLC) -a -o $@ $(OCAMLFLAGS) $(LIBRARIES) $(LIB1_OBJS)
lib1.cmxa: $(LIB1_XOBJS)
	$(OCAMLOPT) -a -o $@ $(OCAMLFLAGS) $(LIB1_XOBJS)


# Common rules

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	rm -f lib1.cma 
	rm -f lib1.cmxa
	rm -f *.cm[iox]
	rm -f */*.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(LOCAL_INCLUDES) \
	Utilities/*.mli Utilities/*.ml \
	Problem/*.ml Problem/*.mli \
	Solver/Tree/*.ml Solver/Tree/*.mli \
	Solver/IteratedDeepening/*.ml Solver/IteratedDeepening/*.mli \
	Solver/IteratedDeepeningTree/*.ml Solver/IteratedDeepeningTree/*.mli > .depend

include .depend


.PHONY: depend doc

# Documentation
html/index.html: $(LIB1_OBJS)
	ocamldoc -verbose -intro intro.ml -short-functors -d html -all-params -charset utf8 -html $(OCAMLFLAGS) \
	Utilities/*.mli Utilities/*.ml \
	Problem/*.ml \
	Solver/Tree/*.ml \
	Solver/IteratedDeepening/*.ml \
	Solver/IteratedDeepeningTree/*.ml  

doc : html/index.html
