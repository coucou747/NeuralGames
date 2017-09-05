
all: tictactoe.native puissance4.native test_bool_neural.native

%.native: *.ml
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml -lib spoc $*.native

error_during_learn_xor.dat: test_bool_neural.native
	./test_bool_neural.native

xor_%.dot: test_bool_neural.native
	./test_bool_neural.native

%.plot.png: %.dat %.plot
	gnuplot $*.plot

%.dot.png: %.dot
	dot $*.dot -Tpng -o $@
clean:
	ocamlbuild -clean

%.o: %.c
	gcc -c $*.c -o $@ -lcublas -lcuda -lcudart \
	-I/usr/local/cuda/include -L/usr/local/cuda/lib64 \
	-I`ocamlc -where` -L`ocamlc -where` \

