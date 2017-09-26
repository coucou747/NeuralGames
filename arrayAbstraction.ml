
module type LinearOperations = sig
  module M : sig
    type t
      
    val add : t -> t -> t
    val diff : t -> t -> t
        
    val from_array : float array array -> t
    val from_array_transposee : float array array -> t
    val to_array : t -> float array array

    val init : int -> int -> (int -> int -> float) -> t
    
    val times : t -> t -> t
    val scalar : t -> float -> unit
    
    val mapf : t -> t
    val mapf' : t -> t
      
    val squaresumdiff : t -> t -> float
  
  end
  module V : sig
    type t
      
    val diff : t -> t -> t
      
    val from_array : float array -> t
    val to_array : t -> float array
        
    val init : int -> (int -> float) -> t
      
    val times : t -> t -> t
    val scalar : t -> float -> unit
    
    val mapf : t -> t
    val mapf' : t -> t

    val squaresumdiff : t -> t -> float

  end
  
  val multiply12 : V.t -> M.t -> V.t
  val multiply21 : M.t -> V.t -> V.t
  val multiply : M.t -> M.t -> M.t
  val multiply_t : M.t -> M.t -> M.t
  val multiply_nt : M.t -> M.t -> M.t

  val scalar_vects_to_map : V.t -> V.t -> M.t
end
module MLArray (A : Activation.Activation) : LinearOperations = struct
  module V = struct
    type t = float array
    let diff = Array.map2 (-.)
    let init = Array.init
    let times =  Array.map2 ( *.)
    let scalar m s =
      let e = Array.length m - 1 in
      for i = 0 to e do
        m.(i) <- m.(i) *. s
      done
    let mapf = Array.map A.f
    let mapf' = Array.map A.f'
    let from_array x = x
    let to_array x = x
    let squaresumdiff v1 v2 = Array.map2 (-.) v1 v2  |> Array.fold_left (fun a b -> a +. b *. b) 0.
  end
  module M= struct
    type t = float array array  
    let add a b =
      if Array.length a != Array.length b || Array.length a.(0) != Array.length b.(0)
      then
        failwith ("wrong sizes : " ^ (string_of_int (Array.length a)) ^ " != " ^
                  (string_of_int (Array.length b)) ^ " || " ^
                  (string_of_int (Array.length a.(0))) ^ " != " ^
                  (string_of_int (Array.length b.(0))) ^ "."
                 );
      Array.map2 (Array.map2 (+.)) a b
        
    let diff a b = Array.map2 (Array.map2 (-.)) a b
    let init x y f = Array.init x (fun x -> Array.init y (fun y ->  f y x))
    let times a b = Array.map2 (Array.map2 ( *.)) a b
    let scalar m s =
      let e = Array.length m - 1 in
      for i = 0 to e do
        let f = Array.length m.(i) - 1 in
        for j = 0 to f do
          m.(i).(j) <- m.(i).(j) *. s
        done
      done
    let from_array x = x
    let to_array x = x
    let from_array_transposee m =
      let x = Array.length (m)
      and y = Array.length (m.(0)) in
      Array.init y (fun y -> Array.init x (fun x ->  m.(x).(y)))
    let squaresumdiff v1 v2 = Array.map2 V.squaresumdiff v1 v2 |> Array.fold_left (+.) 0.
    let mapf m = Array.map (Array.map A.f) m
    let mapf' m = Array.map (Array.map A.f') m
  end
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
  let multiply_t a b = multiply (M.from_array_transposee a) b
  let multiply_nt a b = multiply a (M.from_array_transposee b)
  let scalar_vects_to_map v1 v2 = Array.map (fun d -> Array.map (( *.) d) v2) v1
end

module LacamlMat (A : Activation.Activation) : LinearOperations = struct
  open Lacaml.S
  module V = struct
    type t = Lacaml.S.vec
    let diff v1 v2 = Vec.sub v1 v2
    let times v1 v2 = Vec.mul v1 v2
    let scalar v f = scal f v
    let init n f = Vec.init n (fun i -> f (i - 1))
    let mapf v = Vec.map A.f v
    let mapf' v = Vec.map A.f' v
    let squaresumdiff v1 v2 = Vec.ssqr_diff v1 v2
    let from_array x = Vec.of_array x
    let to_array x = Vec.to_array x
  end
  module M = struct
    type t = Lacaml.S.mat
    let add a b = Mat.add a b
    let diff a b = Mat.sub a b
    let times v1 v2 = Mat.mul v1 v2
    let scalar v f = Mat.scal f v
    let init x y f = Mat.init_cols x y (fun x y -> f (y - 1) (x - 1))
    let mapf mat = Mat.map A.f mat
    let mapf' mat = Mat.map A.f' mat
    let squaresumdiff m1 m2 = Vec.ssqr_diff (Mat.as_vec m1) (Mat.as_vec m2)
    let from_array x = Mat.of_array x
    let from_array_transposee x = Mat.transpose_copy (Mat.of_array x)
    let to_array x = Mat.to_array x
  end 
  let multiply12 v m = gemv ~trans:`T m v
  let multiply21 m v = gemv m v
  let multiply a b = gemm a b
  let multiply_t a b = gemm ~transa:`T a b
  let multiply_nt a b = gemm ~transb:`T a b
  let scalar_vects_to_map v1 v2 =
    Array.map (fun d ->
        let v = copy v1 in
        scal d v;
        v) (Vec.to_array v2) |> Mat.of_col_vecs
end

module CuMat (A : Activation.Activation) : LinearOperations = struct
  open Cudabindings
  module V = struct
    type t = Vec.t
    let diff v1 v2 = Vec.sub v1 v2
    let times v1 v2 = Vec.mul v1 v2
    let scalar v f = Vec.scal f v
    let init = Vec.init
    let mapf = A.cuda_f
    let mapf' = A.cuda_f'
    let squaresumdiff v1 v2 = Vec.ssqr_diff v1 v2
    let from_array x = Vec.of_array x
    let to_array x = Vec.to_array x
  end
  module M = struct
    type t = Mat.t
    let add a b = Mat.add a b
    let diff a b = Mat.sub a b      
    let times v1 v2 = Mat.mul v1 v2
    let scalar v f = Mat.scal f v
    let init x y f = Mat.init_cols x y f
    let mapf = A.cuda_mat_f
    let mapf' = A.cuda_mat_f'
    let squaresumdiff m1 m2 = Mat.ssqr_diff m1 m2
    let from_array x = Mat.of_array x
    let from_array_transposee x = Mat.of_array_transpose x
    let to_array x = Mat.to_array x
  end
  let multiply12 v m = gemv ~trans:`T m v
  let multiply21 m v = gemv m v
  let multiply a b = gemm a b
  let multiply_t a b = gemm ~transa:`T a b
  let multiply_nt a b = gemm ~transb:`T a b
  let scalar_vects_to_map v1 v2 = cublas_vectors_as_matrix_mul v1 v2
end

module type LinearOperationsFunctor = functor (A : Activation.Activation) -> LinearOperations
