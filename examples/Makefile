all: pydump pypp

pydump: pydump.ml
	ocamlfind ocamlopt -o pydump -linkpkg -package pythonlib pydump.ml

pypp: pypp.ml
	ocamlfind ocamlopt -o pypp -linkpkg -package pythonlib pypp.ml

clean:
	rm -f pydump pypp *.cm[ixo] *.o
