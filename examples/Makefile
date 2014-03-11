all: pydump pypp pysimpl

pydump: pydump.ml
	ocamlfind ocamlopt -o pydump -linkpkg -package pythonlib pydump.ml

pypp: pypp.ml
	ocamlfind ocamlopt -o pypp -linkpkg -package pythonlib pypp.ml

pysimpl: pysimpl.ml
	ocamlfind ocamlopt -o pysimpl -linkpkg -package pythonlib pysimpl.ml

clean:
	rm -f pydump pypp pysimpl *.cm[ixo] *.o
