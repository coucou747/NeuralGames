
%.native: %.ml
	ocamlbuild $*.native

error_during_learn_xor.dat: neural.native
	./neural.native

%.dot: neural.native
	./neural.native

%.plot.png: %.dat %.plot
	gnuplot $*.plot

%.dot.png: %.dot
	dot $*.dot -Tpng -o $@
