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
      mutable stats : bool;
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
        stats = false
      } in
      let spec = List.append
          (List.map (fun spec -> spec (fun n ->
               Format.printf "%S = %S ? %b@\n" n name ( String.equal n name );
               if String.equal n name then fun () ->
                 Format.printf "USE %S@\n" name;
                 opt.available <- true
               else fun () ->
                 Format.printf "DONT USE %S@\n" name;
                 opt.available <- false)) specs)
        [
        "-use-"^name, Arg.Unit (fun () -> opt.available <- true), "Enable the "^name^" array abstraction";
        "-stdin-player1", Arg.Unit (fun () -> opt.player1 <- GP.stdin_player), "first player plays from the standard input";
        "-stdin-player2", Arg.Unit (fun () -> opt.player2 <- GP.stdin_player), "second player plays from the standard input";
        "-neural-file-player1", Arg.String (fun s -> opt.player1 <- ai_from_filename s), "loads a neural network for the first player";
        "-neural-file-player2", Arg.String (fun s -> opt.player2 <- ai_from_filename s), "loads a neural network for the second player";
        "-training-file", Arg.String (fun s -> opt.file_to_train <- Some s), "sets the file for the neural network training";
        "-training-iterations", Arg.Int (fun i -> opt.learn_iterations <- i), "sets the number of training loop";
        "-training-steps", Arg.Int (fun i -> opt.learn_steps <- i), "sets the number of training steps (before stats)";
        "-alphabeta-player1", Arg.Int (fun i -> opt.player1 <- GP.negamax_player i), "first player is a negamax of depth n";
        "-alphabeta-player2", Arg.Int (fun i -> opt.player2 <- GP.negamax_player i), "second player is a negamax of depth n";
        "-training-ratio", Arg.Float (fun f -> opt.learn_ratio <- f), "sets the learning ratio";
        "-training-random-percent", Arg.Int (fun i -> opt.training_percent_random <- i), "sets the percent of random moves for learning operations";
        "-stats", Arg.Unit (fun () -> opt.stats <- true), "compute only statistics";
      ] in
      Arg.current := 0;
      Arg.parse spec (fun f -> Format.fprintf Format.err_formatter "What to do with %S@\n" f) descr;
      if not opt.available then
        begin
          Format.printf "don't use %s implementation@\n" name;
          false
        end
      else (begin
          Format.printf "Use %s implementation@\n" name;
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
                  GP.create_ai G.size_neural
              in
              for i = 1 to opt.learn_iterations do
                for j = 1 to opt.learn_steps do
                  GP.learn opt.training_percent_random opt.learn_ratio ai;
                done;
                let s = GP.stats (GP.make_ai_player ai) GP.random_player in
                Format.printf "%d : %a@\n%!" i GP.pp_stats s
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
