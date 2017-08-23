
open Neural
(*
make xor_1.dot.png error_during_learn_xor.plot.png

ce fichier permet de tester les fonctions d'apprentissage et d'évaluation.
Ici, on teste avec 2 couches cachées, de 4 et 4, pour les fonctions xor, and et or.
On fait apprendre 30 réseaux pendant 1000 itérations.

*)

let () =
  let open Tanh in
  let open LayerTanh in
  Random.self_init ();
  let learning_rate = 0.1 in
  let ninputs = 3 in
  let noutput = 3 in
  let examples =
    [
      [0.; 1.; 1.], [1.; 0.; 1.];
      [1.; 1.; 1.], [0.; 1.; 1.];
      [1.; 0.; 1.], [1.; 0.; 1.];
      [0.; 0.; 1.], [0.; 0.; 0.];
    ] in
  (*
     décommentez ça pour tester uniquement une fonction (ici, i=2 donc c'est or)
     il faut aussi changer noutput pour mettre 1
  let examples = List.map (fun (in_, out) ->
      in_, List.filter (fun (i, _) -> i = 2) (List.mapi (fun i j -> i, j) out)
           |> List.map snd) examples in *)
  let examples = List.map (fun (a, b) -> List.map convert01 a, List.map convert01 b) examples in
  let error_channel = open_out "error_during_learn_xor.dat" |> Format.formatter_of_out_channel in
  for i = 1 to 30 do
    let w = init_weights ninputs [4; 4; noutput] in
    let examples = List.map (fun (a, b) -> Array.of_list a, Array.of_list b) examples in
    let w = learns error_channel 1000 learning_rate compute f' w examples in
    Format.fprintf error_channel "@\n@\n";
    let inputs = rand_float_tab 0 ninputs in
    let tab, data = computes compute w inputs in
    let debug_channel = open_out ("xor_"^(string_of_int i)^".dot") |> Format.formatter_of_out_channel in
    debug debug_channel inputs data
  done
    
