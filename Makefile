init : parser.mly lexer.mll main.ml
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -o op parser.cmo lexer.cmo main.ml

run : op
	./op

clean : 
	rm -f *.cmo *.cmi *.cmx *.o *.mli lexer.ml parser.ml op