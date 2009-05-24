.PHONY: clean

tsp: 
	ocamlbuild -libs str tsp.native

clean:
	ocamlbuild -clean

