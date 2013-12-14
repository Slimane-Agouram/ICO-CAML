ZYVA=zyva
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
INCLUDES=
OCAMLFLAGS=$(INCLUDES)
OPTFLAGS=$(INCLUDES) -p
CPP=gcc -E -P -x c 
############ Pour choisir allocation triviale ou pas


OBJS=misc.cmo location.cmo print.cmo lexer.cmo parser.cmo env.cmo\
	interpret.cmo gen.cmo frame.cmo table.cmo gbase.cmo graph.cmo sgraph.cmo\
	trans.cmo iprint.cmo smallset.cmo simul.cmo ass.cmo\
	canon_opt.cmo linear_opt.cmo spim.cmo liveness.cmo spill.cmo\
	partition.cmo alloc.cmo main.cmo
OPTS=$(OBJS:.cmo=.cmx)
SOURCES=$(OBJS:.cmo=.ml)
IOBJS=misc.cmo location.cmo print.cmo lexer.cmo parser.cmo interpret.cmo zyvai.cmo
ICOBJS=misc.cmo location.cmo print.cmo lexer.cmo parser.cmo interpret.cmo\
 gen.cmo frame.cmo env.cmo trans.cmo iprint.cmo smallset.cmo simul.cmo\
 canon.cmo linear.cmo zyvaic.cmo
SOBJS=misc.cmo location.cmo print.cmo lexer.cmo parser.cmo interpret.cmo\
 gen.cmo frame.cmo env.cmo trans.cmo iprint.cmo smallset.cmo simul.cmo\
 canon_opt.cmo linear_opt.cmo ass.cmo table.cmo spim.cmo spill.cmo zyvas.cmo
LOBJS=misc.cmo location.cmo print.cmo lexer.cmo parser.cmo interpret.cmo\
 gen.cmo frame.cmo env.cmo trans.cmo iprint.cmo smallset.cmo simul.cmo\
 canon_opt.cmo linear_opt.cmo ass.cmo table.cmo spim.cmo gbase.cmo graph.cmo\
 sgraph.cmo liveness.cmo spill.cmo zyval.cmo
ISOURCES=$(IOBJS:.cmo=.ml)
ICSOURCES=$(ICOBJS:.cmo=.ml)
SSOURCES=$(SOBJS:.cmo=.ml)
all: zyva

zyvai: ${IOBJS}
	${OCAMLC} -o zyvai ${IOBJS}

zyvaic: ${ICOBJS}
	${OCAMLC} -o zyvaic ${ICOBJS}

zyvas: ${SOBJS}
	${OCAMLC} -o zyvas ${SOBJS}

zyval: ${LOBJS}
	${OCAMLC} -o zyval ${LOBJS}

zyvai.tar: ${IOBJS}
	./mktar -e interpret.ml -e lexer.ml zyvai interpret.mli ${IOBJS} test/Makefile test/*.p test/*.in test/*.ref

zyvaic.tar: ${ICOBJS}
	./mktar -e trans.ml -e lexer.ml zyvaic code.mli ${ICOBJS} test/Makefile test/*.p test/*.in test/*.ref show_flow_graph

zyvas.tar: ${ICOBJS} Makefile
	./mktar -e spim.ml -e lexer.ml zyvas code.mli ${SOBJS} test/Makefile test/*.p test/*.in test/*.ref

zyval.tar: ${ICOBJS} Makefile
	./mktar -e liveness.ml -e lexer.ml zyval code.mli partition.mli ${LOBJS} test/Makefile test/*.p test/*.in test/*.ref

zyva.tar: ${OBJS} Makefile
	./mktar -e alloc.ml -e lexer.ml zyva code.mli ${OBJS} test/Makefile test/*.p test/*.in test/*.ref

all.tar: ${OBJS} Makefile
	./mktar -e parser.ml -e parser.mli -e lexer.ml all code.mli zyvai.ml zyvaic.ml zyvas.ml zyval.ml ${OBJS} test/Makefile test/*.p test/*.in test/*.ref

zyva: ${OBJS}
	${OCAMLC} ${OCAMLFLAGS} -o $@ ${OBJS}

zyva.opt:  ${OPTS}
	${OCAMLOPT} ${OPTFLAGS} -o zyva.opt ${OPTS}

# Version triviale et opt
trivial:
	cp liveness_code.ml liveness.ml
	cp alloc_triv.ml alloc.ml
opt:
	cp liveness_block.ml liveness.ml
	cp alloc_opt.ml alloc.ml

# Test
ok: okfinal
okclean:
	cd test  ; $(MAKE) $(MFLAGS) clean
okfinal:
	cd test ; $(MAKE) $(MFLAGS) final
oki:
	cd test ; $(MAKE) $(MFLAGS) ZYVA=../zyvai i
okic:
	cd test ; $(MAKE) $(MFLAGS) ZYVA=../zyvaic ic
oks:
	cd test ; $(MAKE) $(MFLAGS) ZYVA=../zyvas ZYVAOPTS="-spill" final


# Doc
doc: index.html
	rm -f $(ZYVA) ; $(MAKE) $(ZYVA) NONTRIVIAL=t
	cd test ; $(MAKE) $(MFLAGS) doc

index.html: index.tex usage.txt
	hevea -fix -francais index.tex

usage.txt:$(ZYVA)
	-./$(ZYVA) -help 2> usage.txt
# Common rules
.SUFFIXES:  .mly .mll .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.mly.ml:
	ocamlyacc $<
.mll.ml:
	ocamllex $<
.ml.cmx:
	${OCAMLOPT} ${OPTFLAGS} -c $<

# Clean up
clean::
	rm -f *.tar
	rm -f zyva zyvai zyvaic zyvas zyval zyva.opt *.cm[io] *.o *.cmx
	rm -f *~ *.image.* *.gif tmp.* lexer.ml parser.ml *.spi usage.txt
	cd test ; $(MAKE) $(MFLAGS) clean

cleanall:: clean
	cd test ; $(MAKE) $(MFLAGS) cleanall	

depend: lexer.ml parser.ml
	ocamldep $(INCLUDES) *.ml *.mli > .depend

ifdef NONTRIVIAL
main.cmo: alloc.cmi
endif

include .depend
