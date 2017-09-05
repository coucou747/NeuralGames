
type cublas_vector

external cublas_init : unit -> unit = "cublas_init"
external cublas_shutdown : unit -> unit = "cublas_shutdown"
external cublas_vect_of_array : float array -> cublas_vector = "cublas_vect_of_array";;
external cublas_array_of_vect : cublas_vector -> float array = "cublas_array_of_vect";;
external cublas_vect_scale : cublas_vector -> float -> unit = "cublas_scale";;

cublas_init ();;
at_exit (fun () ->
    Gc.full_major ();
    cublas_shutdown ();
  );;
  
let from_array tab = cublas_vect_of_array tab

let () =
  let cutab = from_array [|0.; 1.; 2.; 3.; 4.;|] in
  cublas_vect_scale cutab 2.;
  Array.iter (Format.printf "%f @\n%!") (cublas_array_of_vect cutab)
