all: pydump pypp pysimplify

pydump: pydump.ml
	ocamlfind ocamlopt -o pydump -linkpkg -package lang_python pydump.ml

pypp: pypp.ml
	ocamlfind ocamlopt -o pypp -linkpkg -package lang_python pypp.ml

pysimplify: pysimplify.ml
	ocamlfind ocamlopt -o pysimplify -linkpkg -package lang_python pysimplify.ml

clean:
	rm -f pydump pypp pysimplify *.cm[ixo] *.o
