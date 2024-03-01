init : parser.mly lexer.mll
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
run : parser.cmo lexer.cmo main.ml
	ocamlc -o op parser.cmo lexer.cmo main.ml
	./op
all : 
	init : parser.mly lexer.mll
	run : parser.cmo lexer.cmo main.ml
