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

module P4 = GamePlay(P4State)(Neural.Tanh)

type o =
  {
    mutable player1 : P4.fplayer;
    mutable player2 : P4.fplayer;
    mutable file_to_train : string option;
    mutable learn_iterations : int;
    mutable learn_steps : int;
    mutable learn_ratio : float;
    mutable stats : bool;
  }


let () =
  Random.self_init ();
  let ai_from_filename s = P4.make_ai_player (P4.load_ai (Scanf.Scanning.from_channel (open_in s))) in
  let progname = Sys.argv.(0) in
  let descr = progname^" options" in
  let opt = { player1 = P4.random_player;
              player2 = P4.random_player;
              file_to_train = None;
              learn_iterations = 0;
              learn_steps = 0;
              learn_ratio = 0.1;
              stats = false
            } in
  let spec = [
    "-stdin-player1", Arg.Unit (fun () -> opt.player1 <- P4.stdin_player), "first player plays from the standard input";
    "-stdin-player2", Arg.Unit (fun () -> opt.player2 <- P4.stdin_player), "second player plays from the standard input";
    "-neural-file-player1", Arg.String (fun s -> opt.player1 <- ai_from_filename s), "loads a neural network for the first player";
    "-neural-file-player2", Arg.String (fun s -> opt.player2 <- ai_from_filename s), "loads a neural network for the second player";
    "-training-file", Arg.String (fun s -> opt.file_to_train <- Some s), "sets the file for the neural network training";
    "-training-iterations", Arg.Int (fun i -> opt.learn_iterations <- i), "sets the number of training loop";
    "-training-steps", Arg.Int (fun i -> opt.learn_steps <- i), "sets the number of training steps (before stats)";
    "-training-ratio", Arg.Float (fun f -> opt.learn_ratio <- f), "sets the learning ratio";
    "-stats", Arg.Unit (fun () -> opt.stats <- true), "compute only statistics";
    
  ] in
  Arg.parse spec (fun f -> Format.fprintf Format.err_formatter "What to do with %S@\n" f) descr;
  if opt.stats then
    let s = P4.stats opt.player1 opt.player2 in
    Format.printf "%a@\n%!" P4.pp_stats s
  else
    let winner = P4.play opt.player1 opt.player2 in
    Format.printf "%a@\n%!" (pp_option P4State.pp_player) winner;
    match opt.file_to_train with
    | None -> ()
    | Some ai_file ->
      let ai =
        try
          P4.load_ai (Scanf.Scanning.from_channel (open_in ai_file))
        with Sys_error _ ->
          Format.printf "Create New AI";
          P4.create_ai [40; 20; 20; 20]
      in
      for i = 1 to opt.learn_iterations do
        for j = 1 to opt.learn_steps do
          P4.learn opt.learn_ratio ai;
        done;
        let s = P4.stats (P4.make_ai_player ai) P4.random_player in
        Format.printf "%d : %a@\n%!" i P4.pp_stats s
      done;
      P4.save_ai (Format.formatter_of_out_channel (open_out ai_file)) ai;
