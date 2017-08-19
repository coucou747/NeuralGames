open StdAddon
    
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
  val floats_of_cell : player -> cell -> float list
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
end

type 'a printer = Format.formatter -> 'a -> unit

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
  val input : Scanf.Scanning.in_channel -> movement

  
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
    
  val play : ?silent:bool -> fplayer -> fplayer -> G.player option
  val stats : fplayer -> fplayer -> stats_t
  val pp_stats : Format.formatter -> stats_t -> unit
    
end = struct
  type fplayer = G.state -> G.player -> G.movement
                                          
  type airef = float array array list ref


  let random_player state player =
    let moves = G.all_moves state player |> Array.of_list in
    moves.(Random.int (Array.length moves))
      
  let stdin_player state player = G.input Scanf.Scanning.stdin
  
  
  module Layer = Neural.Layer(F)  

  let inputs player state =
    Array.of_list
    ( 1. :: (G.floats_of_state player state  |> List.map F.convert01))

  let create_ai layers =
    let ninputs = Array.length (inputs G.p1 (G.state0 ())) in
    let w = Layer.init_weights ninputs (List.append layers [1]) in
    ref w
                                                                                         
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
            let learning_rate = f ns other_player in
            begin
              refw := Neural.expected learning_rate F.f' [| F.invert score |] values_t0 datas_t0;
              learning_rate *. 0.2 (* plus on s'éloigne d'une fin de partie, moins on apprend *)
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

  let pp_stats f (a, b, c) = Format.fprintf f "(W:%d, D:%d, L:%d)" a b c
  
end
