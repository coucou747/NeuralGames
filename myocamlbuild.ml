(*
thanks to :
http://l-lang.org/blog/Incorporating-C-code-in-an-Ocaml-project-using-Ocamlbuild/
*)

open Ocamlbuild_plugin;;

let ocamlfind x = S[A"ocamlfind"; x]
    
let _ = dispatch (function
    | Before_options ->
      Options.ocaml_cflags := ["-g"]
    | Before_rules ->
      rule "nvcc: cu -> o" 
       ~deps:["%.cu"]
       ~prod: "%.o"
       begin
         fun env build ->
           let source = env "%.cu" in
           let tags = tags_of_pathname source++"compile"++"cu" in
           Cmd (S [A "/usr/local/cuda/bin/nvcc"; T tags; A "-c"; P source; A "-o"; Px (env "%.o")])
       end
    | After_rules ->
      List.iter
        (fun pkg ->
           flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
           flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
           flag ["ocaml"; "compile"; "pkg_"^pkg] & S[A"-package"; A pkg];
           flag ["link"; "ocaml"; "pkg_"^pkg] & S[A"-package"; A pkg];
          ())
        ["lacaml"; "bigarray"];
      pdep ["link"] "linkdep" (fun param -> [param]);
      flag ["compile"; "cu"; "use_libcuda"] (S[A"-I/usr/local/cuda/include"; ]);
      flag ["link"; "ocaml"; "native"; "use_libcuda"]
        (S[
            A"-ccopt";
            A"-L/usr/local/cuda/lib64";
            A"-cclib";
            A"-lcublas";
            A"-cclib";
            A"-lcuda";
            A"-cclib";
            A"-lcudart"]);
    | _ -> ())

