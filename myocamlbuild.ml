(*
thanks to :
http://l-lang.org/blog/Incorporating-C-code-in-an-Ocaml-project-using-Ocamlbuild/
*)

open Ocamlbuild_plugin;;

dispatch (function
  | After_rules ->
    pdep ["link"] "linkdep" (fun param -> [param]);
    flag ["c"; "compile"; "use_libcuda"]
      (S[A"-ccopt";
         A"-I/usr/local/cuda/include";]);
    flag ["link"; "ocaml"; "native"; "use_libcuda"]
      (S[
          A"-ccopt";
          A"-L/usr/local/cuda/lib64";
          A"-cclib";
          A"-lcublas";
          A"-cclib";
          A"-lcuda";
          A"-cclib";
          A"-lcudart";]);
  | _ -> ())

