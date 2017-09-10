(*
thanks to :
http://l-lang.org/blog/Incorporating-C-code-in-an-Ocaml-project-using-Ocamlbuild/
*)

open Ocamlbuild_plugin;;

let ocamlfind x = S[A"ocamlfind"; x]
    
let _ = dispatch (function
    | Before_options ->
      Options.ocaml_cflags := ["-g"]
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
      flag ["c"; "compile"; "use_libcuda"]
        (S[A"-ccopt";
           A"-I/usr/local/cuda/include";
          ]);
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

