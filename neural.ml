open ArrayAbstraction
open Activation

module Layer (F : Activation) (L:LinearOperations) = struct
  let make_layer inputs n = let f = F.rand_float n inputs in L.M.init n inputs (fun _ _ -> f ())
  let rec init_weights ninputs = function
    | hd::tl -> make_layer ninputs hd :: init_weights hd tl
    | [] -> []
  let compute inputs weights =
    let sums = L.multiply21 weights inputs in
    sums, L.V.mapf sums
  let computes inputs weights =
    let sums = L.multiply weights inputs in
    sums, L.M.mapf sums
end

let addbiais input =
  Array.init (Array.length input + 1)
    (fun i ->
       if i = 0 then 1.
       else input.(i - 1))

module Make (F : Activation) (L:LinearOperations) : sig

  type neural
  type datat

  val make : int -> int list -> neural
  val compute : neural -> L.V.t -> L.V.t * datat
  val computes : neural -> float array array -> float array array
  val debug : Format.formatter -> L.V.t -> datat -> unit
  val expected : float -> L.V.t -> L.V.t -> datat -> neural
  val learns : ?error_channel:Format.formatter -> int -> float -> neural -> (float array * float array) list -> neural
  val save : Format.formatter -> neural -> unit
  val load : Scanf.Scanning.in_channel -> neural
    
end = struct
  
  module Layer = Layer(F)(L)

  let make = Layer.init_weights
  
  type neural = L.M.t list
  type datat = (L.V.t * L.V.t * L.V.t * L.M.t) list

  let debug debug_channel input datas =
    let input = L.V.to_array input in
    let datas = List.map (fun (a, b, c, d) ->
        L.V.to_array a, L.V.to_array b, L.V.to_array c, L.M.to_array d
      ) datas in
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

  let compute layers input =
    List.fold_left (fun (input, li) layerw ->
        let sums, output = Layer.compute input layerw in
        (output, (sums, output, input, layerw)::li)
      ) (input, []) layers
      
  let computes_with_datas layers inputs =
    List.fold_left (fun (input, li) layerw ->
        let sums, output = Layer.computes input layerw in
        (output, (sums, output, input, layerw)::li)
      ) (inputs, []) layers

  let computes layers inputs = fst (computes_with_datas layers inputs)
    
  let expected learningRate expected values datas =
    let fixlay (weights, error) (sums, values, prev_values, layerw) =
      let fprimesums = L.V.mapf' sums in
      
      let delta = L.V.times fprimesums error in
      L.V.scalar delta learningRate;
      let changes = L.scalar_vects_to_map delta prev_values in
      let prev_error = L.multiply12 delta layerw in
      (L.M.add changes layerw):: weights, prev_error
    in
    let error = L.V.diff expected values in
    let nw, _ = List.fold_left fixlay ([], error) datas
    in nw

  let debug_mat f m =
    let m = L.M.to_array m in
    Format.fprintf f "%dx%d" (Array.length m) (Array.length m.(0))
  
  let learn learning_rate layers inputs expecteds =
    (* Gc.full_major (); *)
    Gc.minor ();
    let values, datas = computes_with_datas layers inputs in
    let fixlay (weights, error) (sums, values, prev_values, layerw) =
      let fprimesums = L.M.mapf' sums in
      let delta = L.M.times fprimesums error in
      L.M.scalar delta learning_rate;
      let changes = L.multiply_nt delta prev_values in
      let prev_error = L.multiply_t layerw delta in
      (L.M.add changes layerw):: weights, prev_error
    in
    let error = L.M.diff expecteds values in
    let nw, _ = List.fold_left fixlay ([], error) datas
    in L.M.squaresumdiff expecteds values, nw
 
  let computes layers input =
    let a = computes layers (L.M.from_array_transposee input) in
    L.M.to_array a
  
  let rec learns ?error_channel n learning_rate weights inputs expecteds =
    if n = 0 then weights
    else
      let error, weights = learn learning_rate weights inputs expecteds in
      begin match error_channel with
        | Some error_channel -> Format.fprintf error_channel "%f@\n%!" error
        | None -> ()
      end;
      learns ?error_channel (n - 1) learning_rate weights inputs expecteds


  let learns ?error_channel n learning_rate weights examples =

    let inputs, expecteds = List.split examples in
    let inputs = Array.of_list inputs in
    let expecteds = Array.of_list expecteds in
    let inputs = L.M.from_array_transposee inputs in
    let expecteds = L.M.from_array_transposee expecteds in
    learns ?error_channel n learning_rate weights inputs expecteds

  let save f w =
    let w = List.map L.M.to_array w in
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
    in List.map L.M.from_array (Array.to_list t)

end
