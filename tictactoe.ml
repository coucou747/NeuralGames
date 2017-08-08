open Ai

let rec pp_list pitem psep f li =
  match li with
  | [] -> ()
  | [item] -> pitem f item
  | hd::tl -> psep f pitem hd (pp_list pitem psep) tl

let pp_sep fmt f ppa a ppb b =  Format.fprintf f fmt ppa a ppb b

let rec seq_append a b li =
  if a > b then li
  else seq_append a (b - 1) (b :: li)

let (--) a b = seq_append a b []
    
module TicState : Game = struct
  
  type player = Remplissage.player
  let p1 = Remplissage.p1
  let other_player = Remplissage.other_player
  
  type state = Remplissage.cell array                
  type movement = int
  type undo_t = movement

  let state0 () = Array.make 9 Remplissage.empty
  
  let line0 = 0 -- 2
  let comb f = List.map (fun l -> List.map (f l) line0 ) line0
  let lines = comb (fun l v -> v + l * 3)
  let colones = comb (fun l v -> v * 3 + l)
  let diag1 = [0; 4; 8]
  let diag2 = [2; 4; 6]
  let indexes_wons = List.flatten [ lines ; colones ; [diag1; diag2] ]
  
  let won state player =
    let c = Remplissage.cell_of_player player in
    List.exists (fun li ->
        List.for_all (fun index -> state.(index) = c) li
      ) indexes_wons
  
  let coords = 1 -- 8

  
  let draw state player =
    List.for_all (fun c -> state.(c) != Remplissage.empty) coords
  
  let all_moves state player = List.filter (fun c -> Remplissage.is_empty state.(c)) coords

  let play state player coords =
    state.(coords) <- Remplissage.cell_of_player player;
    state, coords
    
  let undo state player coords =
    state.(coords) <- Remplissage.empty;
    state

  let li_of_state state =
    List.map (List.map (fun i -> state.(i))) lines
  
  let pp_movement f i = Format.fprintf f "%d" i
  let pp_state f state =
    let li = li_of_state state in
    let pline = pp_list
        (fun f i ->
           Format.fprintf f
             (if i = Remplissage.c1 then "O" else
              if i = Remplissage.c2 then "X" else
                " "
             ))
        (pp_sep "%a %a")
    in
    let pline f li = Format.fprintf f "|%a|" pline li in
    Format.fprintf f "%a@\n" (pp_list pline (pp_sep "%a@\n%a")) li

  let input f = Scanf.bscanf f "%i\n" (fun i -> i)

  let floats_of_state s =
    li_of_state s |> List.flatten |> List.map Remplissage.floats_of_cell |> List.flatten
end

module TicTacToe = GamePlay(TicState)

let _winner = TicTacToe.play TicTacToe.random_player TicTacToe.stdin_player
