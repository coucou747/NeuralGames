
%.native: %.ml
	ocamlbuild $*.native

error_during_learn_xor.dat: neural.native
	./neural.native

%.plot.png: %.dat %.plot
	gnuplot $*.plot
