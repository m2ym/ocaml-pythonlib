all: pydump pypp

pydump: pydump.ml
	ocamlfind ocamlopt -o pydump -linkpkg -package lang_python pydump.ml

pypp: pypp.ml
	ocamlfind ocamlopt -o pypp -linkpkg -package lang_python pypp.ml

clean:
	rm -f pydump pypp *.cm[ixo] *.o
