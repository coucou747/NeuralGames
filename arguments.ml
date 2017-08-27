
open StdAddon

module Make (G : Ai.Game)  (F : Neural.Activation) = struct
  module GP = Ai.GamePlay(G)(F)
      
type o =
  {
    mutable player1 : GP.fplayer;
    mutable player2 : GP.fplayer;
    mutable file_to_train : string option;
    mutable learn_iterations : int;
    mutable learn_steps : int;
    mutable learn_ratio : float;
    mutable stats : bool;
  }


let () =
  Random.self_init ();
  let ai_from_filename s = GP.make_ai_player (GP.load_ai (Scanf.Scanning.from_channel (open_in s))) in
  let progname = Sys.argv.(0) in
  let descr = progname^" options" in
  let opt = { player1 = GP.random_player;
              player2 = GP.random_player;
              file_to_train = None;
              learn_iterations = 0;
              learn_steps = 0;
              learn_ratio = 0.1;
              stats = false
            } in
  let spec = [
    "-stdin-player1", Arg.Unit (fun () -> opt.player1 <- GP.stdin_player), "first player plays from the standard input";
    "-stdin-player2", Arg.Unit (fun () -> opt.player2 <- GP.stdin_player), "second player plays from the standard input";
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
    let s = GP.stats opt.player1 opt.player2 in
    Format.printf "%a@\n%!" GP.pp_stats s
  else
    let winner = GP.play opt.player1 opt.player2 in
    Format.printf "%a@\n%!" (pp_option G.pp_player) winner;
    match opt.file_to_train with
    | None -> ()
    | Some ai_file ->
      let ai =
        try
          GP.load_ai (Scanf.Scanning.from_channel (open_in ai_file))
        with Sys_error _ ->
          Format.printf "Create New AI";
          GP.create_ai [40; 20; 20; 20]
      in
      for i = 1 to opt.learn_iterations do
        for j = 1 to opt.learn_steps do
          GP.learn opt.learn_ratio ai;
        done;
        let s = GP.stats (GP.make_ai_player ai) GP.random_player in
        Format.printf "%d : %a@\n%!" i GP.pp_stats s
      done;
      GP.save_ai (Format.formatter_of_out_channel (open_out ai_file)) ai;


end
