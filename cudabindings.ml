
type cublas_vector
type cublas_matrix

external cublas_init : unit -> unit = "cublas_init"
external cublas_shutdown : unit -> unit = "cublas_shutdown"
  
external cublas_vect_of_array : float array -> cublas_vector = "cublas_vect_of_array"
external cublas_array_of_vect : cublas_vector -> float array = "cublas_array_of_vect"

external cublas_array_of_matrix : bool -> cublas_matrix -> float array array = "cublas_array_of_matrix";;
external cublas_matrix_of_array : bool -> float array array ->  cublas_matrix = "cublas_matrix_of_array";;
  
external cublas_vect_copy : cublas_vector -> cublas_vector = "cublas_vect_copy"
external cublas_matrix_copy : cublas_matrix -> cublas_matrix = "cublas_matrix_copy"

external cublas_vect_vect_incr : cublas_vector -> cublas_vector -> float -> unit = "cublas_vect_incr"
external cublas_matrix_matrix_incr : cublas_matrix -> cublas_matrix -> float -> unit = "cublas_matrix_incr"

external cublas_matrix_vector_mul :
 cublas_matrix -> cublas_vector -> float -> float -> int -> cublas_vector = "cublas_mul_matrix_vector"
  
external cublas_vectors_as_matrix_mul :
 cublas_vector -> cublas_vector -> cublas_matrix = "cublas_mul_vects_as_matrix"
  
external cublas_matrix_matrix_mul :
 cublas_matrix -> cublas_matrix -> float -> int -> cublas_matrix = "cublas_mul_matrix"
  
external cublas_vect_scale : cublas_vector -> float -> unit = "cublas_scale"
external cublas_vect_mul : cublas_vector -> cublas_vector -> cublas_vector = "cublas_mul"

external cublas_ssqr : cublas_vector -> float = "cublas_ssqr";;
external cublas_vec_tanh : cublas_vector -> cublas_vector = "cublas_vec_tanh";;
external cublas_vec_sigmoid : cublas_vector -> cublas_vector = "cublas_vec_sigmoid";;

external cublas_mat_tanh : cublas_matrix -> cublas_matrix = "cublas_mat_tanh";;
external cublas_mat_sigmoid : cublas_matrix -> cublas_matrix = "cublas_mat_sigmoid";;

external cublas_vec_tanh' : cublas_vector -> cublas_vector = "cublas_vec_tanh2";;
external cublas_vec_sigmoid' : cublas_vector -> cublas_vector = "cublas_vec_sigmoid2";;

let () =
  cublas_init ();
  at_exit (fun () ->
    Gc.full_major ();
    cublas_shutdown ();
  )


let trans_of_int trans = match trans with
  | `N -> 0
  | `T -> 1

module Vec = struct
  type t = cublas_vector
  let sub v2 v1 = let v3 = cublas_vect_copy v2 in cublas_vect_vect_incr v1 v3 (-1.); v3
  let add v1 v2 = let v3 = cublas_vect_copy v2 in cublas_vect_vect_incr v1 v3 1.; v3
  let mul = cublas_vect_mul
  let of_array = cublas_vect_of_array
  let to_array = cublas_array_of_vect
  let init n f = Array.init n f |> of_array
  let scal f v = cublas_vect_scale v f
  let copy = cublas_vect_copy
  let ssqr_diff v1 v2 = sub v1 v2 |> cublas_ssqr
  let tanH = cublas_vec_tanh
  let tanH' = cublas_vec_tanh'
  let sigmoid = cublas_vec_sigmoid
  let sigmoid' = cublas_vec_sigmoid'
end

module Mat = struct
  type t = cublas_matrix
  let add m1 m2 = let m3 = cublas_matrix_copy m2 in cublas_matrix_matrix_incr m1 m3 1.; m3
  let copy = cublas_matrix_copy
  let of_array_transpose = cublas_matrix_of_array true
  let of_array = cublas_matrix_of_array false
  let to_array = cublas_array_of_matrix false
  let init x y f = Array.init x (fun x -> Array.init y (fun y -> f x y)) |> of_array
  let init_cols x y f = Array.init x (fun x -> Array.init y (fun y -> f y x)) |> of_array
  let map f vec = Array.map (Array.map f) (to_array vec) |> of_array
  let sigmoid = cublas_mat_sigmoid
  let tanH = cublas_mat_tanh
end

let gemm a b = cublas_matrix_matrix_mul a b 1. 0
let gemv ?(trans=`N) a b =
  let trans = trans_of_int trans in
  cublas_matrix_vector_mul a b 1. 0. trans
