open ArrayAbstraction
open StdAddon

module Make (G : Ai.Game)  (F : Activation.Activation) = struct

  type 'gp_fplayer o =
    {
      mutable available : bool;
      mutable player1 : 'gp_fplayer;
      mutable player2 : 'gp_fplayer;
      mutable file_to_train : string option;
      mutable learn_iterations : int;
      mutable learn_steps : int;
      mutable learn_ratio : float;
      mutable training_percent_random : int;
      mutable database_size : int;
      (* methods *)
      mutable stats : bool;
      mutable play : bool;
      mutable multi_train_nlearn : int;
      mutable multi_train_ngames : int;
      mutable training_from : int;
    }
  
  let instantiate specs default name (module M: LinearOperationsFunctor) =
    let module GP = (Ai.GamePlay(G)(F)(M(F))) in
    let main () =
      let ai_from_filename s = GP.make_ai_player (GP.load_ai (Scanf.Scanning.from_channel (open_in s))) in
      let progname = Sys.argv.(0) in
      let descr = progname^" options" in
      let opt = {
        available = default;
        player1 = GP.random_player;
        player2 = GP.random_player;
        file_to_train = None;
        learn_iterations = 0;
        learn_steps = 0;
        learn_ratio = 0.1;
        training_percent_random = 50;
        database_size = -1;
        stats = false;
        play = false;
        multi_train_nlearn = 0;
        multi_train_ngames = 0;
        training_from = 1;
      } in
      let spec = List.append
          (List.map (fun spec -> spec (fun n ->
               if String.equal n name then fun () -> opt.available <- true
               else fun () -> opt.available <- false)) specs)
        [
        "-use-"^name, Arg.Unit (fun () -> opt.available <- true), "Enable the "^name^" array abstraction";
        "-stdin-player1", Arg.Unit (fun () -> opt.player1 <- GP.stdin_player), "first player plays from the standard input";
        "-stdin-player2", Arg.Unit (fun () -> opt.player2 <- GP.stdin_player), "second player plays from the standard input";
        "-neural-file-player1", Arg.String (fun s -> opt.player1 <- ai_from_filename s), "loads a neural network for the first player";
        "-neural-file-player2", Arg.String (fun s -> opt.player2 <- ai_from_filename s), "loads a neural network for the second player";
        "-training-file", Arg.String (fun s -> opt.file_to_train <- Some s), "sets the file for the neural network training";
        "-training-iterations", Arg.Int (fun i -> opt.learn_iterations <- i), "sets the number of training loop";
        "-training-steps", Arg.Int (fun i -> opt.learn_steps <- i), "sets the number of training steps (before stats)";
        "-database-size", Arg.Int (fun i -> opt.database_size <- i), "sets the size of the database for endgame training";
        "-alphabeta-player1", Arg.Int (fun i -> opt.player1 <- GP.negamax_player i), "first player is a negamax of depth n";
        "-alphabeta-player2", Arg.Int (fun i -> opt.player2 <- GP.negamax_player i), "second player is a negamax of depth n";
        "-training-ratio", Arg.Float (fun f -> opt.learn_ratio <- f), "sets the learning ratio";
        "-training-random-percent", Arg.Int (fun i -> opt.training_percent_random <- i), "sets the percent of random moves for learning operations";
        "-stats", Arg.Unit (fun () -> opt.stats <- true), "compute only statistics";
        "-multi-train-nlearn", Arg.Int (fun i -> opt.multi_train_nlearn <- i), "sets the number of learning session in case of a multi learn";
        "-multi-train-ngames", Arg.Int (fun i -> opt.multi_train_ngames <- i), "sets the number of games used to create the database in case of a multi learn";
        "-training-from", Arg.Int (fun i -> opt.training_from <- i), "restrict the distance between the end of the game and the training positions (default=1 : 1 move away from the end of the game)";
        
      ] in
      Arg.current := 0;
      Arg.parse spec (fun f -> Format.fprintf Format.err_formatter "What to do with %S@\n" f) descr;
      if not opt.available then false
      else (begin
          if opt.stats then begin
            let s = GP.stats opt.player1 opt.player2 in
            Format.printf "%a@\n%!" GP.pp_stats s
          end;
          if opt.play then begin
            let winner = GP.play opt.player1 opt.player2 in
            Format.printf "%a@\n%!" (pp_option G.pp_player) winner
          end;
            match opt.file_to_train with
            | None -> ()
            | Some ai_file ->
              let ai =
                try
                  GP.load_ai (Scanf.Scanning.from_channel (open_in ai_file))
                with Sys_error _ ->
                  Format.printf "Create New AI";
                  GP.create_ai G.size_neural
              in
              if opt.database_size > 0 then begin
                if opt.learn_iterations = 0 then
                  Format.printf "use the option -training-iterations to train the database.@\n%!"
                else begin
                  Format.printf "training the neural network for endgames with a database of size : %d and %d iterations@\n%!" opt.database_size opt.learn_iterations;
                  GP.learn_endgames ~error_channel:Format.std_formatter opt.learn_iterations opt.database_size opt.learn_ratio ai
                end
              end
              else
                for i = 1 to opt.learn_iterations do
                  for j = 1 to opt.learn_steps do
                    if opt.multi_train_nlearn > 0 then
                      GP.multilearn
                        ~nlearn:opt.multi_train_nlearn
                        ~ngames:opt.multi_train_ngames
                        opt.training_from
                        opt.training_percent_random opt.learn_ratio ai
                    else GP.learn opt.training_percent_random opt.learn_ratio ai
                  done;
                  let s = GP.stats (GP.make_ai_player ai) GP.random_player in
                  Format.printf "%4d : %a@\n%!" i GP.pp_stats s
                done;
              GP.save_ai (Format.formatter_of_out_channel (open_out ai_file)) ai
        end; true)
    in main;;

let () =
  let arraysimpls = [
    "Lacaml", (module LacamlMat : LinearOperationsFunctor);
    "CuMat", (module CuMat : LinearOperationsFunctor);
    "MLArray", (module MLArray : LinearOperationsFunctor)] in
  Random.self_init ();
  let specs = List.map (fun (name, _) apply ->
      "-use-"^name, Arg.Unit (apply name), "Enable the "^name^" array abstraction"
    ) arraysimpls in
  let gps = List.mapi (fun i (name, (module M : LinearOperationsFunctor)) ->
      instantiate specs (i = 0) name (module M))  arraysimpls in
  ignore (List.fold_left (fun done_ f -> if not done_ then f () else true) false gps)
    
end
