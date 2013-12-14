SPIM=spim -notrap -file 
ZYVA=../zyva
ZYVAOPTS=
TESTCOM=../zyva


TEST= fact.out fib.out tri.out mul.out fact2.out fact3.out fact4.out\
       fact5.out mul2.out quick.out \
      bignum.out triangle.out arit.out args.out tregs.out tregs2.out\
      matmult.out atree.out bool.out transp.out power.out poweriter.out\
      mulbin.out
CODE=$(TEST:.out=.spi)
TESTCODE=$(TEST:.out=.spiout)
SOURCE=$(TEST:.out=.p)
LIGNES=$(TEST:.out=.cr)
LIVE=$(TEST:.out=.live)
ok: i ic final
zyvai:
	$(MAKE) $(TEST) TESTCOM="../$@"

code: $(CODE)
live: $(LIVE)
$(CODE) $(LIVE): $(ZYVA)

i ic:
	$(MAKE) $(TEST) TESTCOM="$(ZYVA) -$@ $(ZYVAOPTS)"

final:
	$(MAKE) $(CODE) "ZYVA=$(ZYVA)" "ZYVAOPTS=$(ZYVAOPTS)"
	$(MAKE) $(TESTCODE) "ZYVA=$(ZYVA)" "ZYVAOPTS=$(ZYVAOPTS)"

cleancode:
	rm -f $(CODE)

clean: cleancode
	rm -f  *~ $(LIGNES) tests.tex *.ps *.gif

cleanall: clean
	rm -f index.html
	rm -rf $(LIVE)

.p.out:
	@echo -n "Test: $< " with $(TESTCOM) " "
	@$(TESTCOM) $< < $*.in | diff - $*.ref
	@echo OK

.spi.spiout:
	@echo -n "Test: $< " with $(SPIM) " "
	@$(SPIM) $< < $*.in | tail +5 | diff - $*.ref
	@echo OK
.p.live:
	@$(ZYVA) $(ZYVAOPTS) -liveness $< | (tee $@ > /dev/null)

.p.spi:
	@echo -n "Compiling: $< " with $(ZYVA) $(ZYVAOPTS) " "
	@($(ZYVA) $(ZYVAOPTS) $< | tee $@ > /dev/null) || (rm $@ && exit 1)
	@echo OK

doc:
	$(MAKE) index.html

index.html: index.tex tests.tex
	hevea -francais -fix index

tests.tex: $(SOURCE) $(CODE) $(LIVE) $(LIGNES)
	cat $(LIGNES) > tests.tex

.p.cr:
	@echo "\\source{$*.p} & \\source{$*.spi} & \\source{$*.in} & \\source{$*.ref} & \\source{$*.live} \\\\" > $@

.SUFFIXES: .spi .p .in .out .ref .spiout .cr .live
