
module type Activation = sig
  type t = float -> float
  val f : float -> float
  val f' : float-> float
  val rand_float_tab : int -> int -> float array
  val convert01 : float -> float
  val invert : float -> float
  val max : float
  val min : float
  val neutral : float
end

module Sigmoid : Activation = struct
  type t = float -> float
  let rfloat () = (Random.float 2.) -. 1.

  let f x = 1. /. (1. +. exp (-. x))
  let f' x =
    let fx = f x in
    fx *. (1. -. fx)
  let rand_float_tab nn n =
    let s =  sqrt (1. /. float_of_int (nn + n)) in
    Array.init n (fun _ -> 4. *. rfloat () *. s)
  let convert01 x = x
  let invert x = 1. -. x
  let min = 0.
  let max = 1.
  let neutral = 0.5
end

module Tanh : Activation = struct
  type t = float -> float
  let rfloat () = (Random.float 2.) -. 1.

  let f x = tanh x
  let f' x = 1. -. (tanh x) *. (tanh x)
  let rand_float_tab nn n = 
    let s =  sqrt (1. /. float_of_int (nn + n)) in
    Array.init n (fun _ -> 4. *. rfloat () *. s)
  let convert01 x = x *. 2. -. 1.
  let invert x = -. x
    let min = -.1.
  let max = 1.
  let neutral = 0.
end

module Layer (F : Activation) = struct
  let make_layer inputs n =
    Array.init n (fun _ -> F.rand_float_tab n inputs)
  let rec init_weights ninputs = function
    | hd::tl -> make_layer ninputs hd :: init_weights hd tl
    | [] -> []
  let compute inputs weights =
    let nneurons = Array.length weights in
    let ninputs = Array.length inputs in
    assert (ninputs = (Array.length weights.(0)));
    let sums = Array.make nneurons 0. in
    for n = 0 to nneurons - 1 do
      for i = 0 to ninputs - 1 do
        sums.(n) <- sums.(n) +. inputs.(i) *. weights.(n).(i)
      done;
    done;
    sums, Array.map F.f sums
end

let addbiais input =
  Array.init (Array.length input + 1)
    (fun i ->
       if i = 0 then 1.
       else input.(i - 1))

module Make (F : Activation) : sig
  type neural
  type datat
  val make : int -> int list -> neural
  val computes : neural -> float array -> float array * datat
  val debug : Format.formatter -> float array -> datat -> unit
  val expected : float -> float array -> float array -> datat -> neural
  val learns : Format.formatter -> int -> float -> neural -> (float array * float array) list -> neural
  val save : Format.formatter -> neural -> unit
  val load : Scanf.Scanning.in_channel -> neural
    
end = struct
  
  module Layer = Layer(F)

  let make = Layer.init_weights
  
  type neural = float array array list
  type datat = (float array * float array * float array * float array array) list
  let computes layers input =
    List.fold_left (fun (input, li) layerw ->
        let sums, output = Layer.compute input layerw in
        (output, (sums, output, input, layerw)::li)
      ) (input, []) layers

let debug debug_channel input datas =
  Format.fprintf debug_channel "@[<v 2>digraph {@\nsplines=line;@[<v 2>@\nsubgraph cluster_input {";
  Array.iteri (fun i v ->
      Format.fprintf debug_channel "@\ni_%d[label=\"i_%d %f\" color=\"lightblue2\" style=\"filled\" shape=\"diamond\" ]" i i v
    ) input;
  Format.fprintf debug_channel "@]@\n}";
  let player i f j = Format.fprintf f "l%d_%d" i j in
  List.iteri (fun layer (sums, values, _prev_values, _layerw) ->
      Format.fprintf debug_channel "@[<v 2>@\nsubgraph cluster_%d {" layer;
      Array.iteri (fun j value ->      
          Format.fprintf debug_channel "@\n%a[label=\"f(%f)=@\n%f\" color=\"darksalmon\" style=\"filled\"]" (player layer) j value values.(j)
        ) sums;
      Format.fprintf debug_channel "@]@\n}";
    ) (List.rev datas);
  ignore (List.fold_left (fun (layer, namei) (sums, values, _prev_values, layerw) ->
      Array.iteri (fun j tabv ->
          for k = 0 to Array.length tabv - 1 do
            let pw f () = Format.fprintf f "w_%d_%d_%d" layer j k in
            Format.fprintf debug_channel "@\n%a[label=\"%f\" shape=\"box\" style=\"filled\" color=\"lightgrey\"]@\n%a -> %a@\n%a -> %a [color=\"grey\"]"
              pw ()
              tabv.(k)
              namei k pw ()
              pw () (player layer) j
          done
        ) layerw;
      (layer + 1, player layer)
    )
    (0, (fun f i -> Format.fprintf f "i_%d" i))
    (List.rev datas));
  Format.fprintf debug_channel "@]@\n}@\n"

let mapmat2 f m1 m2 =
  assert( Array.length m1 = Array.length m2);
  assert( Array.length m1.(0) = Array.length m2.(0));
  Array.mapi (fun i m -> Array.mapi (fun j v -> f v m2.(i).(j)) m ) m1
    
let maparr2 f a1 a2 = 
  assert( Array.length a1 = Array.length a2);
  Array.mapi (fun i v -> f v a2.(i)) a1
let add = mapmat2 (+.)
let substract = maparr2 (-.)
let dot = maparr2 ( *. )
let scalar m s = Array.map (fun v -> v *. s) m

let transpose mat = Array.init (Array.length mat.(0)) (fun i -> Array.init (Array.length mat) (fun j -> mat.(j).(i)))

let multiply mat1 mat2 =
  let s1 = Array.length mat1 in
  let s2 = Array.length mat2.(0) in
  let s3 = Array.length mat1.(0) in
  assert (s3 = Array.length mat2);
  Array.init s1 (fun i -> Array.init s2 (fun j ->
          let sum = ref 0. in
          for k = 0 to s3 do
            sum := !sum +. mat1.(i).(k) *. mat2.(k).(j)
          done;
          !sum
    ))
let multiply21 mat tab =
  Array.map (fun vect ->
      snd (Array.fold_left (fun (indice, sum) value ->
          (indice + 1, sum +. tab.(indice) *. value)) (0, 0.) vect)
    ) mat

let expected learningRate expected values datas=
  let fixlay
      (weights, (error : float array))
      (sums, values, (prev_values:float array), (layerw:float array array)) =
    let delta = dot (Array.map F.f' sums) error in
    let changes = Array.mapi (fun i d -> scalar prev_values (d *. learningRate) ) delta in
    let prev_error = multiply21 (transpose layerw) delta in
      (add changes layerw):: weights, prev_error
  in
  let error = substract expected values in
  let nw, _ = List.fold_left fixlay ([], error) datas
  in nw

let learn learning_rate weights examples =
  List.fold_left (fun (sum_error, weights) (inputs, expectedv) ->
      let values, datas = computes weights inputs in
      
      let gerror = substract values expectedv |> Array.map (fun x -> x *. x) |> Array.fold_left (+.) 0. in
      sum_error +. gerror, expected learning_rate expectedv values datas
    ) (0., weights) examples

let rec learns error_channel n learning_rate weights examples =
  if n = 0 then weights
  else
    let error, weights = learn learning_rate weights examples in
    Format.fprintf error_channel "%f@\n" error;
    learns error_channel (n - 1) learning_rate weights examples

  let save f w =
    Format.fprintf f "%d@\n" (List.length w);
    List.iter (fun layer ->
        Format.fprintf f "%d %d " (Array.length layer) (Array.length layer.(0));
        Array.iter (fun weights ->
            Array.iter (fun weight -> Format.fprintf f "%Lx " (Int64.bits_of_float weight)) weights;
          ) layer;
        Format.fprintf f "@\n%!"
      )
      w

  let load f =
    let t = Scanf.bscanf f "%d " (fun nlayers ->
        Array.init nlayers (fun _ ->
            Scanf.bscanf f "%d %d " (fun rows cols ->
                Array.init rows (fun _ ->
                    Array.init cols (fun _ ->
                        Scanf.bscanf f "%Lx " Int64.float_of_bits
                      )))))
    in Array.to_list t
      
end
