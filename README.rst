AI - TD + AB
========

Projet de temporal difference learning.
---------------------------------------

Ce dépot est un petit projet d'étude autodidacte autour des réseaux neuronaux.

Le but était de jouer autour des réseaux neuronaux et des jeux type tictactoe, puissance4, peut-être échecs, et pourquoi pas backgammon. L'algorithme que j'ai mis en place dans cette expérimentation est du temporal difference learning.

Pour mesurer l'efficacité du TD learning, il y a aussi une petite implémentation d'un alphabeta classique.

On peut aussi jouer en console contre mes IAs.

Fichiers
--------

* ``neural.ml`` contient une librairie de réseaux neuronaux
* ``ai.ml`` contient l'essentiel des algorithmes
* ``arrayAbstraction.ml`` contient une abstraction des matrices et vecteurs utilisés pour les réseaux neuronnaux, ainsi que trois implémentation de cette abstraction : une qui utilise lacaml (blas pour ocaml), une qui utilise des tableaux ocaml standard, et une qui utilise cublas (blas en cuda, par nvidia)
* ``cudabindings.ml`` est lié à ``cublas.c``, ces deux fichiers fournissent un binding entre cublas et ocaml
* ``arguments.ml`` est un foncteur qui permet de gérer les arguments de la ligne de commande
* ``puissance4.ml`` et ``tictactoe.ml`` contiennent les règles des deux jeux.
* ``test_linear.ml`` est une forme de test unitaire pour l'abstraction sur les tableaux.
* ``test_bool_neural.ml`` est une forme de test unitaire pour les réseaux neuronaux.

Compile
-------

Pour compiler le jeu de tictactoe, faire::
  make tictactoe.native

Utilisation
-----------
  
training ::
    ./tictactoe.native -training-file tictactoe_nn.dat -training-iterations 1000 -training-steps 100 -training-ratio 0.1

endgame training ::
    ./tictactoe.native -training-file tictactoe_nn.dat -training-ratio 0.05 -use-CuMat -database-size 128 -training-iterations 1000

multi learn ::
      ./tictactoe.native -training-file tictactoe_nn.dat -training-iterations 10 -training-steps 1 -training-ratio 0.1 -training-random-percent 20 -multi-train-nlearn 1 -multi-train-ngames 20 -training-from 2
    
mesure contre l'alphabeta::
  ./tictactoe.native -alphabeta-player1 10 -neural-file-player2 tictactoe_nn.dat -stats

Si vous souhaitez préciser l'implémentation des tableaux à utiliser, vous pouvez ajouter une option::
  ./puissance4.native -training-file puissance4_ai.nn -training-iterations 10 -training-steps 10 -training-ratio 0.1 -use-Lacaml
