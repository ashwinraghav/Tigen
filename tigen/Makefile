# Wes Weimer - Graduate Programming Languages - Makefile
#
# Common targets: 
#	make test	(copies test-sources/ to test/, runs tigen)
#	make eval	(produces report.txt)
#	make clean
PATH_TO_Z3_OCAML	= $(PWD)/../z3/ocaml/
PATH_TO_Z3_LIBS		= $(PWD)/../z3/lib/
PATH_TO_CIL		= $(PWD)/../cil-1.3.7/obj/[A-Za-z]*/ 

OCAMLOPT  = ocamlopt -I $(PATH_TO_Z3_OCAML) -I $(PATH_TO_CIL)
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

TIGEN_OBJS = \
	utils.cmx \
	tigen.cmx 

INSTRUMENT_OBJS = \
	utils.cmx \
	instrument.cmx 

TIGEN_LIBS = \
	-cclib "-L $(PATH_TO_Z3_LIBS)" \
	-cclib $(PATH_TO_Z3_LIBS)/libz3.so \
	-cclib -lcamlidl \
	-cclib -lrt \
	-cclib -lgomp \
	str.cmxa \
	unix.cmxa \
	z3.cmxa \
	cil.cmxa

all : tigen instrument

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

$(PATH_TO_Z3_LIBS)/libz3.so:
	echo "You must download and unpack Z3 first." 
	exit 1

tigen: $(TIGEN_OBJS) $(PATH_TO_Z3_LIBS)/libz3.so 
	$(OCAMLOPT) -o tigen $(TIGEN_LIBS) $(TIGEN_OBJS)

instrument: $(INSTRUMENT_OBJS) $(PATH_TO_Z3_LIBS)/libz3.so 
	$(OCAMLOPT) -o instrument $(TIGEN_LIBS) $(INSTRUMENT_OBJS)

test: test-sources instrument tigen
	$(RM) -rf test
	mkdir test 
	cd test-sources ; for d in */ ; do mkdir -p ../test/$$d ; for f in $$d/* ; do ../instrument $$f >> ../test/$$f.c ; ../tigen ../test/$$f.c ; done ; done

eval: clean test 
	$(RM) -rf report.txt
	cd test ; for d in */ ; do cd $$d ; $(RM) -rf output ; for t in test[0-9]*.c ; do echo $$d $$t ; gcc *.i.c $$t > /dev/null 2> /dev/null ; ./a.out >> output 2> /dev/null ; done ; echo -n *.i.c : You reached `grep ________ output | sort | uniq | wc -l` " " >> ../../report.txt ; grep MAX *.i.c >> ../../report.txt ; cd .. ; done  
	cat report.txt

clean:
	$(RM) tigen instrument *.cmi *.cmx report.txt *.o
	$(RM) -rf test
