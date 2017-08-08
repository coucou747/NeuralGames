
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
  val floats_of_cell : cell -> float list
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

  let floats_of_cell i = match i with
    | 0 -> [0.; 0.]
    | -1 -> [1.; 0.]
    | 1 -> [0.; 1.]
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

  
  val floats_of_state : state -> float list
end

module GamePlay (G : Game) : sig
  type fplayer
  val random_player : fplayer
  val stdin_player : fplayer
  val play : fplayer -> fplayer -> G.player option
end = struct
  type fplayer = G.state -> G.player -> G.movement

  let random_player state player =
    let moves = G.all_moves state player |> Array.of_list in
    moves.(Random.int (Array.length moves))

  let stdin_player state player = G.input Scanf.Scanning.stdin
  
  let play fplayer1 fplayer2 =
    let s0 = G.state0 () in
      Format.printf "%a%!@\n" G.pp_state s0;
    let rec f state player fplayer1 fplayer2 =
      let move = fplayer1 state player in
      let ns, _ = G.play state player move in
      Format.printf "%a%!@\n" G.pp_state ns;
      if G.won ns player then Some player
      else if G.draw ns player then None
      else f ns (G.other_player player) fplayer2 fplayer1
    in f s0 G.p1 fplayer1 fplayer2
  
end
