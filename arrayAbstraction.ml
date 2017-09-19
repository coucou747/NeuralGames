
module type LinearOperations = sig
  type vector
  type matrix
  val add : matrix -> matrix -> matrix
  val diff : vector -> vector -> vector
  val v_times : vector -> vector -> vector
  val scalar : vector -> float -> unit
  val multiply12 : vector -> matrix -> vector
  val multiply21 : matrix -> vector -> vector
  val multiply : matrix -> matrix -> matrix
  val init : int -> (int -> float) -> vector
  val init_matrix : int -> int -> (int -> int -> float) -> matrix
  val mapf : vector -> vector
  val mapf' : vector -> vector
  val map2f : matrix -> matrix
  val squaresumdiff : vector -> vector -> float
  val scalar_vects_to_map : vector -> vector -> matrix
  val from_array : float array -> vector
  val to_array : vector -> float array
  val from_array2 : float array array -> matrix
  val from_array2_transposee : float array array -> matrix
  val to_array2 : matrix -> float array array
end
module MLArray (A : Activation.Activation) : LinearOperations = struct
  type vector = float array
  type matrix = float array array

  let init = Array.init
  let init_matrix x y f = Array.init x (fun x -> Array.init y (fun y ->  f y x))
  
  let add a b =
    if Array.length a != Array.length b || Array.length a.(0) != Array.length b.(0)
    then
      failwith ("wrong sizes : " ^ (string_of_int (Array.length a)) ^ " != " ^
                (string_of_int (Array.length b)) ^ " || " ^
                (string_of_int (Array.length a.(0))) ^ " != " ^
                (string_of_int (Array.length b.(0))) ^ "."
               );

    Array.map2 (Array.map2 (+.)) a b
      
  let diff = Array.map2 (-.)
  let v_times =  Array.map2 ( *.)
      
  let scalar m s =
    let e = Array.length m - 1 in
    for i = 0 to e do
      m.(i) <- m.(i) *. s
    done
    
  let multiply12 tab mat =
    if Array.length mat != Array.length tab then
      failwith ("wrong sizes : " ^  (string_of_int (Array.length mat)) ^ " != "
                                     ^ (string_of_int (Array.length tab ))
                ^ "(" ^  (string_of_int (Array.length mat.(0))) ^ ")"
               );
    Array.mapi (fun i _ ->
        snd (Array.fold_left (fun (j, sum) v -> j + 1, sum +. v *. mat.(j).(i)
                             ) (0, 0.) tab)
      ) mat.(0)
      
  let multiply21 mat tab =
    if Array.length mat.(0) != Array.length tab then
      failwith ("wrong sizes : " ^  (string_of_int (Array.length mat.(0))) ^ " != "
                                     ^ (string_of_int (Array.length tab ))
                ^ "(" ^  (string_of_int (Array.length mat)) ^ ")"
               );
    Array.map (fun submat ->
        snd (Array.fold_left (fun (j, sum) v -> j + 1, sum +. v *. submat.(j)
                             ) (0, 0.) tab)
      ) mat

  let multiply a b = Array.map (fun v -> multiply12 v b) a
  
  let scalar_vects_to_map v1 v2 = Array.map (fun d -> Array.map (( *.) d) v2) v1

  let mapf = Array.map A.f
  let mapf' = Array.map A.f'
  let map2f m = Array.map (Array.map A.f) m

  let from_array x = x
  let to_array x = x
  let from_array2 x = x
  let to_array2 x = x
  let from_array2_transposee m =
    let x = Array.length (m)
    and y = Array.length (m.(0)) in
    Array.init y (fun y -> Array.init x (fun x ->  m.(x).(y)))

  let squaresumdiff v1 v2 = Array.map2 (-.) v1 v2  |> Array.fold_left (fun a b -> a +. b *. b) 0.
end

module LacamlMat (A : Activation.Activation) : LinearOperations = struct
  open Lacaml.S
  type vector = vec
  type matrix = mat

  let diff v1 v2 = Vec.sub v1 v2
    
  let add a b = Mat.add a b
      
  let v_times v1 v2 = Vec.mul v1 v2
  let scalar v f = scal f v
  let multiply12 v m = gemv ~trans:`T m v
  let multiply21 m v = gemv m v
  let multiply a b = gemm a b
  
  let init n f = Vec.init n (fun i -> f (i - 1))
  let init_matrix x y f = Mat.init_cols x y (fun x y -> f (y - 1) (x - 1))
  let mapf v = Vec.map A.f v
  let mapf' v = Vec.map A.f' v
  let map2f mat = Mat.map A.f mat
  let squaresumdiff v1 v2 = Vec.ssqr_diff v1 v2
  let scalar_vects_to_map v1 v2 =
    Array.map (fun d ->
        let v = copy v1 in
        scal d v;
        v) (Vec.to_array v2) |> Mat.of_col_vecs
   
  let from_array x = Vec.of_array x
  let to_array x = Vec.to_array x
  let from_array2 x = Mat.of_array x
  let from_array2_transposee x = Mat.transpose_copy (Mat.of_array x)
  let to_array2 x = Mat.to_array x
end

module CuMat (A : Activation.Activation) : LinearOperations = struct
  open Cudabindings

  type vector = Vec.t
  type matrix = Mat.t

  let diff v1 v2 = Vec.sub v1 v2
    
  let add a b = Mat.add a b
      
  let v_times v1 v2 = Vec.mul v1 v2
  let scalar v f = Vec.scal f v
  let multiply12 v m = gemv ~trans:`T m v
  let multiply21 m v = gemv m v
  let multiply a b = gemm a b
  
  let init = Vec.init
  let init_matrix x y f = Mat.init_cols x y f
  let mapf = A.cuda_f
  let mapf' = A.cuda_f'
  let map2f = A.cuda_mat_f
  let squaresumdiff v1 v2 = Vec.ssqr_diff v1 v2
  let scalar_vects_to_map v1 v2 = cublas_vectors_as_matrix_mul v1 v2
    
  let from_array x = Vec.of_array x
  let to_array x = Vec.to_array x
  let from_array2 x = Mat.of_array x
  let from_array2_transposee x = Mat.of_array_transpose x
  let to_array2 x = Mat.to_array x
  
end

module type LinearOperationsFunctor = functor (A : Activation.Activation) -> LinearOperations
