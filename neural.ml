
module type Activation = sig
  val f : float -> float
  val f' : float-> float
end

module Sigmoid : Activation = struct
  let beta = 5.
  let f x = 1. /. (1. +. exp (-. x *. beta))
  let f' x = beta *. f x *. (1. -. f x)
end

module Tanh : Activation = struct
  let f x = tanh x
  let f' x = 1. -. (tanh x) *. (tanh x)
end

module Layer (F : Activation) = struct
  let compute inputs weights =
    let nneurons = Array.length weights in
    let ninputs = Array.length inputs in
    assert (ninputs = (Array.length weights.(0)));
    let sums = Array.make nneurons 0. in
    for n = 0 to nneurons - 1 do
      for i = 0 to ninputs - 1 do
        sums.(n) <- sums.(n) +. inputs.(i) *. weights.(n).(i)
      done
    done;
    Array.map F.f sums
end

let rand_float_tab n = Array.init n (fun _ -> (Random.float 2.) -. 1.)
let make_layer inputs n = Array.init n (fun _ -> rand_float_tab inputs)
let rec init_weights ninputs = function
  | hd::tl -> make_layer ninputs hd :: init_weights hd tl
  | [] -> []

let compute compute layers input =
  List.fold_left compute input layers

module LayerSigmoid = Layer(Sigmoid)


let w = init_weights 19 [20; 1]
let () =
  let tab = compute LayerSigmoid.compute w (rand_float_tab 19) in
  Format.printf "result=%f@\n%!" tab.(0)
