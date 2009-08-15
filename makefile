all: lib/tree_ut lib/haar_ut haar_coint

lib/tree_ut: lib/tree.cmo lib/tree_ut.ml
	ocamlc -I lib/ lib/tree.cmo lib/tree_ut.ml -o $@ 
lib/haar_ut: lib/tree.cmo lib/haar.cmo lib/haar_ut.ml
	ocamlc -I lib/ lib/tree.cmo lib/haar.cmo lib/haar_ut.ml -o $@ 
haar_coint : lib/tree.cmo lib/haar.cmo haar_coint.ml 
	ocamlc -I lib/ str.cma lib/tree.cmo lib/haar.cmo haar_coint.ml -o $@  

lib/tree.cmo : lib/tree.mli lib/tree.ml
	ocamlc -c -I lib lib/tree.mli
	ocamlc -c -I lib lib/tree.ml

lib/haar.cmo : lib/haar.mli lib/haar.ml
	ocamlc -c -I lib lib/haar.mli
	ocamlc -c -I lib lib/haar.ml

clear:
	rm lib/*.cmo lib/*.cmi lib/tree_ut lib/haar_ut haar_coint
	
