all: pydump 

pydump: pydump.ml
	ocamlfind ocamlopt -o pydump -linkpkg -package lang_python pydump.ml

clean:
	rm -f pydump *.cm[ixo] *.o
