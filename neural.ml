
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

let add =  Array.map2 (Array.map2 (+.))
let scalar m s = Array.map (fun v -> v *. s) m

let multiply12 tab mat =
  Array.mapi (fun i _ ->
      snd (Array.fold_left (fun (j, sum) v -> j + 1, sum +. v *. mat.(j).(i)
         ) (0, 0.) tab)
    ) mat.(0)
    
let multiply21 mat tab =
  Array.map (fun submat ->
      snd (Array.fold_left (fun (j, sum) v -> j + 1, sum +. v *. submat.(j)
         ) (0, 0.) tab)
    ) mat

module Layer (F : Activation) = struct
  let make_layer inputs n =
    Array.init n (fun _ -> F.rand_float_tab n inputs)
  let rec init_weights ninputs = function
    | hd::tl -> make_layer ninputs hd :: init_weights hd tl
    | [] -> []
  let compute inputs weights =
    let sums = multiply21 weights inputs in
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

  let computes layers input =
    List.fold_left (fun (input, li) layerw ->
        let sums, output = Layer.compute input layerw in
        (output, (sums, output, input, layerw)::li)
      ) (input, []) layers

  let expected learningRate expected values datas=
    let fixlay (weights, error) (sums, values, prev_values, layerw) =
      let delta = Array.mapi (fun i e -> e *. F.f' sums.(i)) error in
      let changes = Array.mapi (fun i d -> scalar prev_values (d *. learningRate) ) delta in
      let prev_error = multiply12 delta layerw in
      (add changes layerw):: weights, prev_error
    in
    let error = Array.map2 (-.) expected values in
    let nw, _ = List.fold_left fixlay ([], error) datas
    in nw

  let learn learning_rate weights examples =
    List.fold_left (fun (sum_error, weights) (inputs, expectedv) ->
        let values, datas = computes weights inputs in
        let gerror = Array.map2 (-.) values expectedv |> Array.fold_left (fun a b -> a +. b *. b) 0. in
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
