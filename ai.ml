open StdAddon

type 'a printer = Format.formatter -> 'a -> unit
    
module Remplissage : sig
  type cell
  type player
  val other_cell : cell -> cell
  val other_player : player -> player
  val is_empty : cell -> bool
  val empty : cell
  val p1 : player
  val p2 : player
  val c1 : cell
  val c2 : cell
  val cell_of_player : player -> cell
  val float_of_cell : player -> cell -> float
  val floats_of_cell : player -> cell -> float list
  val pp_player : player printer
  val pp_cell : cell printer
end = struct
  type cell = int
  type player = int
  let other_cell a = -a
  let other_player a = -a
  let is_empty a = a = 0

  let empty = 0
  let p1 = 1
  let p2 = -1
  let c1 = 1
  let c2 = -1
  let cell_of_player a = a

  let floats_of_cell player i = match i with
    | 0 -> [0.; 0.]
    | (1 | -1) as m -> if player = m then [1.; 0.] else [0.; 1.]
    | _ -> assert false
      
  let float_of_cell player i = match i with
    | 0 -> 0.5
    | (1 | -1) as m -> if player = m then 0. else 1.
    | _ -> assert false

  let pp_player f p = Format.fprintf f "%d" p
  let pp_cell f p = Format.fprintf f "%d" p
end

module type Game = sig
  type player
  val p1 : player
  val other_player : player -> player
  
  type state
  type movement
  type undo_t
  
  val state0 : unit -> state
  val won : state -> player ->  bool
  val draw : state -> player ->  bool
  val all_moves : state -> player -> movement list
  val play : state -> player -> movement -> state * undo_t
  val undo : state -> player -> undo_t -> state
  val pp_state : state printer
  val pp_movement : movement printer
  val pp_player : player printer
  val input : Scanf.Scanning.in_channel -> movement

  val size_neural : int list
  
  val floats_of_state : player -> state -> float list
end

module GamePlay (G : Game) (F : Neural.Activation) : sig
  type fplayer
  type airef
  type stats_t
    
  val random_player : fplayer
  val stdin_player : fplayer

  val create_ai : int list -> airef
  val make_ai_player : airef -> fplayer
  val learn : float -> airef -> unit

  val save_ai : Format.formatter -> airef -> unit
  val load_ai : Scanf.Scanning.in_channel -> airef
    
  val play : ?silent:bool -> fplayer -> fplayer -> G.player option
  val stats : fplayer -> fplayer -> stats_t
  val pp_stats : Format.formatter -> stats_t -> unit
    
end = struct
  type fplayer = G.state -> G.player -> G.movement
                                          
  type airef = float array array list ref

  let random_player state player =
    let moves = G.all_moves state player |> Array.of_list in
    let n = Array.length moves in
    assert (n != 0);
    moves.(Random.int n)
      
  let stdin_player state player = G.input Scanf.Scanning.stdin
  
  
  module Layer = Neural.Layer(F)  

  let inputs player state =
    Array.of_list
    ( 1. :: (G.floats_of_state player state  |> List.map F.convert01))

  let create_ai layers =
    let ninputs = Array.length (inputs G.p1 (G.state0 ())) in
    let w = Layer.init_weights ninputs (List.append layers [1]) in
    ref w

  let save_ai f w =
    Format.fprintf f "%d@\n" (List.length (!w));
    List.iter (fun layer ->
        Format.fprintf f "%d %d " (Array.length layer) (Array.length layer.(0));
        Array.iter (fun weights ->
            Array.iter (fun weight -> Format.fprintf f "%Lx " (Int64.bits_of_float weight)) weights;
          ) layer;
        Format.fprintf f "@\n%!"
      )
      (!w)

  let load_ai f =
    let t = Scanf.bscanf f "%d " (fun nlayers ->
        Array.init nlayers (fun _ ->
            Scanf.bscanf f "%d %d " (fun rows cols ->
                Array.init rows (fun _ ->
                    Array.init cols (fun _ ->
                        Scanf.bscanf f "%Lx " Int64.float_of_bits
                      )))))
  in ref (Array.to_list t)
                                                                                         
  let move_score_ai_player refw state player =
    let moves = G.all_moves state player in
    let moves = List.map (fun move ->
        let state, undo = G.play state player move in
        let floats = inputs player state in (* current player, next state *)
        let _state = G.undo state player undo in
        let tab, _data = Neural.computes Layer.compute (!refw) floats in
        move, tab.(0)
      ) moves in
    fold1 (fun  (movea, scorea) (moveb, scoreb) ->
        if scorea > scoreb then (movea, scorea) else (moveb, scoreb)
      ) moves
  
  let make_ai_player refw state player =
    let move, _score =  move_score_ai_player refw state player in move
    
  let learn learning_rate refw =
    let rec f state player =
      let other_player = G.other_player player in
      if (Random.int 2) = 0 then
        (* on force l'IA a explorer d'autres morceaux de l'arbre que les "meilleurs" coups *)
        let move = random_player state player in
        let ns, _ = G.play state player move in
        if G.won ns player then learning_rate
        else if G.draw ns player then learning_rate
        else f ns other_player
      else
        begin
          let move, score =  move_score_ai_player refw state player in
          let inputs_t0 = inputs other_player state in
          let values_t0, datas_t0 = Neural.computes Layer.compute (!refw) inputs_t0 in
          let ns, _ = G.play state player move in
          let tdend score_t0 score_t1 =
              refw := Neural.expected learning_rate F.f' [| score_t0 |] values_t0 datas_t0;
              let inputs_t1 = inputs player ns in
              let values_t1, datas_t1 = Neural.computes Layer.compute (!refw) inputs_t1 in
              refw := Neural.expected learning_rate F.f' [| score_t1 |] values_t1 datas_t1;
              learning_rate
          in
          if G.won ns player then tdend F.min F.max 
          else if G.draw ns player then tdend F.neutral F.neutral
          else
            let learning_rate = (f ns other_player) *. 0.5 in
            begin
              refw := Neural.expected learning_rate F.f' [| F.invert score |] values_t0 datas_t0;
              learning_rate
            end
        end
    in ignore (f (G.state0 ()) G.p1)
    
  let play ?(silent=false) fplayer1 fplayer2 =
    let s0 = G.state0 () in
      if not silent then Format.printf "%a%!@\n" G.pp_state s0;
    let rec f state player fplayer1 fplayer2 =
      let move = fplayer1 state player in
      let ns, _ = G.play state player move in
      if not silent then  Format.printf "%a%!@\n" G.pp_state ns;
      if G.won ns player then Some player
      else if G.draw ns player then None
      else f ns (G.other_player player) fplayer2 fplayer1
    in f s0 G.p1 fplayer1 fplayer2

  type stats_t = int * int * int
  let stats fplayer1 fplayer2 =
    let (++) (a, b, c) (d, e, f) = (a + d, b +e, c + f) in
    let rec f n p1 p2 s1 s2 score =
      if n = 0 then score else
      let score = match play ~silent:true p1 p2 with
        | None -> score ++ (0, 1, 0)
        | Some p -> if p = G.p1 then score ++ s1 else score ++ s2
      in f (n - 1) p2 p1 s2 s1 score
    in f 100 fplayer1 fplayer2 (1, 0, 0) (0, 0, 1) (0, 0, 0)

  let pp_stats f (a, b, c) = Format.fprintf f "(W:%d, D:%d, L:%d score=%d/%d)" a b c ( a * 2 + b) ((a + b + c) * 2)
  
end
