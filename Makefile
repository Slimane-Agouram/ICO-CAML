TARGET = pseudopascal

pseudopascal: lexer.cmo parser.cmo print.cmo main.cmo 
	ocamlc -o $@ lexer.cmo parser.cmo print.cmo main.cmo 

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo $(TARGET)
	rm -rf parser.ml parser.mli lexer.ml lexer.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

include .depend
