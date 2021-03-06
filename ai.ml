open StdAddon
open ArrayAbstraction

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

  val learn_draw : bool
  
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

  val alpha0 : float
  val beta0 : float
  val eval : state -> player -> float

  val floats_of_state : player -> state -> float list
end

module GamePlay (G : Game) (F : Activation.Activation)  (L:LinearOperations) : sig
  type fplayer
  type airef
  type stats_t
    
  val random_player : fplayer
  val stdin_player : fplayer

  val create_ai : int list -> airef
  val make_ai_player : airef -> fplayer
  val learn : int -> float -> airef -> unit
  val multilearn : ?nlearn:int -> ?ngames:int -> int -> int -> float -> airef -> unit
  val learn_endgames : ?error_channel:Format.formatter -> int -> int -> float -> airef -> unit

  val save_ai : Format.formatter -> airef -> unit
  val load_ai : Scanf.Scanning.in_channel -> airef
    
  val play : ?silent:bool -> fplayer -> fplayer -> G.player option
  val stats : fplayer -> fplayer -> stats_t
  val pp_stats : Format.formatter -> stats_t -> unit
  val negamax_player : int -> fplayer
end = struct
  type fplayer = G.state -> G.player -> G.movement  

  module N = Neural.Make(F)(L)                                     
  type airef = N.neural ref

  let random_player state player =
    let moves = G.all_moves state player |> Array.of_list in
    let n = Array.length moves in
    assert (n != 0);
    moves.(Random.int n)
      
  let stdin_player state player = G.input Scanf.Scanning.stdin
  
  let inputs_li player state = 1. :: (G.floats_of_state player state  |> List.map F.convert01)
  let inputs player state = Array.of_list (inputs_li player state)

  let create_ai layers =
    let ninputs = Array.length (inputs G.p1 (G.state0 ())) in
    let w = N.make ninputs (List.append layers [1]) in
    ref w

  let save_ai f x = N.save f (!x)
  let load_ai f = ref (N.load f)

  let rec negamax p state player alpha beta =
    if G.won state player then -1.
    else if G.draw state player then 0.
    else if p = 0 then G.eval state player
    else
      let p = p - 1 in
      let player = G.other_player player in
      let moves = G.all_moves state player in
      let score, _alpha, _beta = List.fold_left (fun (m, alpha, beta) move ->
          if beta < alpha then (m, alpha, beta) else
          let state, undo = G.play state player move in
          let score = -. negamax p state player (-.beta) (-.alpha) in
          let _state = G.undo state player undo in max m score, max m alpha, beta
        ) (alpha, alpha, beta) moves in
      score
        
  let negamax_player p state player =
    let moves = G.all_moves state player |> shuffle in
    let moves = List.map (fun move ->
        let state, undo = G.play state player move in
        let score = -. negamax p state player G.alpha0 G.beta0 in
        let _state = G.undo state player undo in move, score
      ) moves in
    fst (fold1 (fun  (movea, scorea) (moveb, scoreb) ->
        if scorea > scoreb then (movea, scorea) else (moveb, scoreb)
      ) moves)
                                                                                         
  let move_score_ai_player refw state player =
    let moves = G.all_moves state player in
    let moves = List.map (fun move ->
        let state, undo = G.play state player move in
        let floats = inputs player state in (* current player, next state *)
        let _state = G.undo state player undo in
        move, floats
      ) moves in
    let moves, floats = List.split moves in
    let floats = Array.of_list floats in
    let values = N.computes (!refw) floats in
    let moves = List.mapi (fun i m -> m, values.(0).(i)) moves in
    fold1 (fun  (movea, scorea) (moveb, scoreb) ->
        if scorea > scoreb then (movea, scorea) else (moveb, scoreb)
      ) moves
  
  let make_ai_player refw state player =
    let move, _score =  move_score_ai_player refw state player in move
    
  let learn training_percent_random learning_rate refw =
    let expected lr score values datas = N.expected lr (L.V.from_array [| score |]) values datas in
    let rec f state player =
      let other_player = G.other_player player in
      if (Random.int 100) < training_percent_random then
        (* on force l'IA a explorer d'autres morceaux de l'arbre que les "meilleurs" coups *)
        let move = random_player state player in
        let ns, _ = G.play state player move in
        if G.won ns player then learning_rate
        else if G.draw ns player then learning_rate
        else f ns other_player
      else
        begin
          let move, score =  move_score_ai_player refw state player in
          let inputs_t0 = inputs other_player state |> L.V.from_array in
          let ns, _ = G.play state player move in
          let tdend score_t0 score_t1 =
              let values_t0, datas_t0 = N.compute (!refw) inputs_t0 in
              refw := expected learning_rate score_t0 values_t0 datas_t0;
              let inputs_t1 = inputs player ns  |> L.V.from_array in
              let values_t1, datas_t1 = N.compute (!refw) inputs_t1 in
              refw := expected learning_rate score_t1 values_t1 datas_t1;
              learning_rate
          in
          if G.won ns player then tdend F.min F.max
          else if G.draw ns player then tdend F.neutral F.neutral
          else
            let learning_rate = (f ns other_player) *. 0.5 in
            let values_t0, datas_t0 = N.compute (!refw) inputs_t0 in
            refw := expected learning_rate (F.invert score) values_t0 datas_t0;
            learning_rate
        end
    in ignore (f (G.state0 ()) G.p1)

  let multilearn ?(nlearn=100) ?(ngames=10) learn_from training_percent_random learning_rate
      refw =
    let adddb input score db =
      if List.exists (fun (a, b) -> a = input) db then db
      else (input, [|score|]) :: db
    in
    let addinput p inputs score db =
      if p > learn_from then db else adddb inputs score db
    in
    let rec f state player db =
      let other_player = G.other_player player in
      if (Random.int 100) < training_percent_random then
        let moves = G.all_moves state player in
        let ns, p, db =
          List.fold_left (fun (state, p, db) move ->
            let ns, undo = G.play state player move in
            let inputs_t0 = inputs other_player state in
            let tdend score_t0 score_t1 =
              let inputs_t1 = inputs player ns in
              ns, 0, (adddb inputs_t0 score_t0 (adddb inputs_t1 score_t1 db))
            in
            let ns, p', db =
              if G.won ns player then tdend F.min F.max
              else if G.draw ns player then tdend F.neutral F.neutral
              else f ns other_player db
            in
            let ns = G.undo ns player undo in
            ns, min p p', db
          ) (state, 10000, db) moves
        in ns, p + 1, db
      else
        begin
          let move, score =  move_score_ai_player refw state player in
          let inputs_t0 = inputs other_player state in
          let ns, undo = G.play state player move in
          let tdend score_t0 score_t1 =
            let ns = G.undo ns player undo in
            let inputs_t1 = inputs player ns in
            ns, 0, (adddb inputs_t0 score_t0 (adddb inputs_t1 score_t1 db))
          in
          if G.won ns player then tdend F.min F.max
          else if G.draw ns player then tdend F.neutral F.neutral
          else
            let ns, p, db = f ns other_player db in
            let ns = G.undo ns player undo in
            let score = F.invert score in
            let db = addinput p inputs_t0 score db in
            ns, p+1, db
        end
    in
    let rec mkonedb db n = match n with (* let's build a db of "n" games*)
      | 0 -> db
      | n ->
        let _, _, db = f (G.state0 ()) G.p1 db in
        mkonedb db (max 0 (n - 1))
    in let db = mkonedb [] ngames in
    if db != [] then
      let ndb = List.length db |> float_of_int in
      Format.printf "Multi learn %f@\n" ndb;
      refw  := N.learns nlearn (learning_rate /. ndb) !refw db

  let learn_endgames ?error_channel nlearn db_size learning_rate refw =
    let rec find_end n state player tolearn =
      let other_player = G.other_player player in
      let move = random_player state player in
      let ns, _ = G.play state player move in
      let endgame score0 score1 =
          let inputs_t0 = inputs other_player state in
          let inputs_t1 = inputs player ns in
          database n ((inputs_t0, [|score0|])::(inputs_t1, [|score1|] )::tolearn) in
      if G.won ns player then endgame F.min F.max
      else if G.draw ns player then if G.learn_draw then endgame F.neutral F.neutral else tolearn
      else find_end n ns other_player tolearn
    and database n tolearn =
      if n = 0 then tolearn
      else find_end (n - 1) (G.state0 ()) G.p1 tolearn
    in
    let db = database db_size [] in
    refw  := N.learns ?error_channel nlearn learning_rate !refw db

    
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

  let pp_stats f (a, b, c) = Format.fprintf f "(W:%3d, D:%3d, L:%3d score=%3d/%3d)" a b c ( a * 2 + b) ((a + b + c) * 2)
  
end
