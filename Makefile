CC = gcc

TARFILES = Makefile scanner.mll parser.mly ast.mli calc.ml

OBJS = ast.cmo parser.cmo scanner.cmo calc.cmo
ALLOBJS = ast.cmo parser.cmo scanner.cmo compile.cmo execute.cmo pltree.cmo


calc : $(OBJS)
	ocamlc -o calc $(OBJS)

pltree: $(ALLOBJS)
	ocamlc -o pltree $(ALLOBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : menhir
menhir:
	menhir -v parser.mly

hello: hello.o tree.o

hello.o: hello.c tree.h

hello.c: calc hello.tree
	cat hello.tree | ./calc -c > hello.c


calculator.tar.gz : $(TARFILES)
	cd .. && tar zcf calculator/calculator.tar.gz $(TARFILES:%=calculator/%)

.PHONY : clean
clean :
	rm -f calc pltree parser.ml parser.mli scanner.ml
	rm -f *.cmo *.cmi hello hello.c tree tree.o hello.o
	rm -f *.automaton *.conflicts

# Generated by ocamldep *.ml *.mli
calc.cmo : scanner.cmo parser.cmi ast.cmi
calc.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
read.cmo : scanner.cmo parser.cmi ast.cmi
read.cmx : scanner.cmx parser.cmx ast.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
ast.cmi :
parser.cmi : ast.cmi
calc.cmo: scanner.cmo parser.cmi ast.cmi
calc.cmx: scanner.cmx parser.cmx ast.cmi
parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmi
