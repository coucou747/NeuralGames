open Ai

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

  let li_of_state lines state =
    List.map (List.map (fun i -> state.(i))) lines
  
  let pp_movement f i = Format.fprintf f "%d" i
  let pp_state f state =
    let li = li_of_state lines state in
    let pline =
      Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f " ")
        (fun f i ->
           Format.fprintf f
             (if i = Remplissage.c1 then "O" else
              if i = Remplissage.c2 then "X" else
                " "
             ))
    in
    let pline f li = Format.fprintf f "|%a|" pline li in
    Format.fprintf f "%a@\n"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "@\n") pline)
      li

  let input f = Scanf.bscanf f "%i\n" (fun i -> i)

  let select li =
    let li = List.sort compare li in List.hd li



    
  let all_li = [
    lines;
    colones;
    (List.map List.rev lines);
    (List.map List.rev colones);
  ]
  let all_li = List.flatten [
      all_li;
      (List.map List.rev all_li);
      (List.map (List.map List.rev) all_li)
    ]
  
  let floats_of_state player s =
    let all_li = List.map (fun l -> li_of_state l s) all_li in
    List.map 
      (fun li -> List.flatten li |> List.map (Remplissage.floats_of_cell player) |> List.flatten)
      all_li |> select
end

module TicTacToe = GamePlay(TicState)(Neural.Tanh)

(* let _winner = TicTacToe.play TicTacToe.random_player TicTacToe.stdin_player *)

let () =
  let ai_file =  "tictactoe_ai.nn" in
  let ai =
    try
      TicTacToe.load_ai (Scanf.Scanning.from_channel (open_in ai_file))
    with Sys_error _ ->
      Format.printf "Create New AI";
      TicTacToe.create_ai [36; 18; 18]
  in
  for i = 1 to 100 do
    for j = 1 to 100 do
      TicTacToe.learn 0.01 ai;
    done;
    let s = TicTacToe.stats (TicTacToe.make_ai_player ai) TicTacToe.random_player in
    Format.printf "%d : %a@\n%!" i TicTacToe.pp_stats s
  done;
  TicTacToe.save_ai (Format.formatter_of_out_channel (open_out ai_file)) ai;
