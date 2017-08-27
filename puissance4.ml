open Ai
open StdAddon

module P4State : Game = struct

  type colone = {
    indice : int;
    mutable height : int;
    content : Remplissage.cell array
  }
  
  type player = Remplissage.player
  let pp_player = Remplissage.pp_player

  let p1 = Remplissage.p1
  let other_player = Remplissage.other_player
  type state = colone array                
  type movement = int
  type undo_t = movement

  let mk_column i = { indice = i; height = 0; content = Array.make 6 Remplissage.empty }
  
  let state0 () = Array.init 7 mk_column

  let draw state player = Array.for_all (fun c -> c.height = 6) state

  let pp_movement f i = Format.fprintf f "%d" i
  let input f = Scanf.bscanf f "%i\n" (fun i -> i)

  let play column player =
    column.content.(column.height) <- (Remplissage.cell_of_player player);
    column.height <- column.height + 1
  let undo column =
    column.height <- column.height - 1;
    column.content.(column.height) <- Remplissage.empty
    
    
  let play state player coords =
    play state.(coords) player;
    state, coords
    
  let undo state player coords =
    undo state.(coords);
    state

  let columns = 0 -- 6
  let lines = 0 -- 5

  let in_board (x, y) = x >= 0 && x < 7 && y >= 0 && y < 6
  
  let won_li =
    let cells = List.map (fun y -> List.map (fun x -> x, y) columns) lines |> List.flatten in
    let deltas = [0, 1; 1, 0; 1, 1; 1, -1] in
    let arrows = List.map (fun (dx, dy) ->
        List.map (fun d -> dx * d, dy * d) (0 -- 3)
      ) deltas in
    let cellsarrow = List.map (fun (x, y) ->
        List.map (List.map (fun (x2, y2) -> x + x2, y + y2)) arrows
      ) cells |> List.flatten in
    let cellsarrow = List.filter (List.for_all in_board) cellsarrow
    in cellsarrow

  let won_li =
    Array.init 7 (fun c -> Array.init 6 (fun l ->
        List.filter (List.exists ((=) (c, l))) won_li
      ))
  
  let won state player =
    let player_cell = Remplissage.cell_of_player player in
    Array.exists (fun col ->
        col.height > 0 &&
        List.exists (
          List.for_all (fun (x, y) ->
              state.(x).content.(y) = player_cell
            )) won_li.(col.indice).(col.height - 1)
      ) state
    
  
  let all_moves state player = List.filter (fun c -> state.(c).height < 6) columns

  let lili state =
    List.map (fun l ->
        List.map (fun c -> state.(c).content.(l)) columns
      ) lines
  
  let pp_state f state =
    Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "@\n")
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "|")
         (fun f i ->
            Format.fprintf f
              (if i = Remplissage.c1 then "O" else
               if i = Remplissage.c2 then "X" else
                 " ")
         )) f (lili state)
  

  let floats_of_state player state =
    let li = lili state in
    let li' = List.map List.rev li in
    let li = if li < li' then li else li' in
    List.flatten (List.map (Remplissage.floats_of_cell player) (List.flatten li))
      
end

module Main = Arguments.Make(P4State)(Neural.Tanh)
