open ArrayAbstraction

module Test (L : LinearOperations) = struct
  
  let a = L.V.init 7 (fun i -> float_of_int (i * i + 10))
  let b = L.V.init 7 (fun i -> float_of_int i)
  let c = L.V.times a b
  let d = L.V.diff a b
  let e = L.V.mapf (L.V.init 7 (fun i -> float_of_int (i * i + 10) /. 100. ))
  let e' = L.V.mapf' (L.V.init 7 (fun i -> float_of_int (i * i + 10) /. 100. ))
  let f = L.V.squaresumdiff a b
  let g = L.V.init 5 (fun i -> float_of_int (i * i * i))
      
  let ma = L.M.init 5 7 (fun x y -> float_of_int (x * 100 + y))
  let mb = L.M.init 5 7 (fun x y -> float_of_int (x * 10 + y * y * 15))
  let mc = L.M.add ma mb
  let mc' = L.M.diff ma mb
  let mc'' = L.M.times ma mb
  
  let me = L.M.init 2 4 (fun x y -> float_of_int (x * 10 + y))
  let me' = L.M.init 4 2 (fun x y -> float_of_int (x * 10 + y))
  let mf = L.M.init 4 5 (fun x y -> float_of_int (x * 10 + y))
  let mf' = L.M.init 5 4 (fun x y -> float_of_int (x * 10 + y))
  let mg = L.multiply me mf
  let mg' = L.multiply_t me' mf
  let mg'' = L.multiply_nt me mf'

  let score = L.M.squaresumdiff ma mb
  
  let h = L.multiply21 ma a
  let i = L.multiply12 g ma
  let md = L.scalar_vects_to_map e g
  let me = L.M.mapf md
  let me' = L.M.mapf' md


  let t = [| [| 1.; 2.; 3.|]; [| 4.; 5.; 6.|] |]
  let mh = L.M.from_array_transposee t
  let mi = L.M.from_array t
end

module Unit (A : LinearOperations) (B : LinearOperations) = struct
  module TA = Test(A)
  module TB = Test(B)
      
  let pp_vec f v =
    Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f " ")
      (fun f i -> Format.fprintf f "%.3f" i)
      f (Array.to_list v)

  let pp_mat f v =
    Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "@\n")
      pp_vec
      f (Array.to_list v)
           
  let cmp v1 v2 = Array.map2 (fun a b -> let c = a -. b in c *. c) v1 v2 |>
                  Array.for_all (fun f -> f < 0.00001)
                    
  let vec_eq str v1 v2 =
    let v1 = A.V.to_array v1
    and v2 = B.V.to_array v2 in
    let cmp = cmp v1 v2 in
    Format.printf "@[<v10> %S%a =@\n%a@] : %b@\n" str
      pp_vec v1 pp_vec v2
      cmp;
    assert cmp
      
  let mat_eq str v1 v2 =
    let v1 = A.M.to_array v1
    and v2 = B.M.to_array v2 in
    let cmp = Array.map2 cmp v1 v2 |> Array.fold_left (&&) true in
    Format.printf "@[<v10>%S %a =@\n%a@] : %b@\n" str
      pp_mat v1 pp_mat v2
      cmp;
    assert cmp
      
  let float_eq str f1 f2 =
    Format.printf "%S %.3f = %.3f : %b@\n" str f1 f2 (f1 = f2);
    assert (f1 = f2)
    
  let () =
    vec_eq "init" TA.a TB.a;
    vec_eq "init" TA.b TB.b;
    vec_eq "times" TA.c TB.c;
    vec_eq "diff" TA.d TB.d;
    A.V.scalar TA.a 2.;
    B.V.scalar TB.a 2.;
    vec_eq "Scalar" TA.a TB.a;
    vec_eq "mapf" TA.e TB.e;
    vec_eq "mapf'" TA.e' TB.e';
    float_eq "squaresumdiff" TA.f TB.f;
    float_eq "squaresumdiff_mat" TA.score TB.score;
    vec_eq "init" TA.g TB.g;
    mat_eq "init_matrix" TA.ma TB.ma;
    mat_eq "init_matrix" TA.mb TB.mb;
    mat_eq "add" TA.mc TB.mc;
    mat_eq "diff" TA.mc' TB.mc';
    mat_eq "m_times" TA.mc'' TB.mc'';

    A.M.scalar TA.mc'' 2.;
    B.M.scalar TB.mc'' 2.;
    mat_eq "scalar" TA.mc'' TB.mc'';
    

    vec_eq "multiply21" TA.h TB.h;
    vec_eq "multiply12" TA.i TB.i;
    mat_eq "scalar_vects_to_map" TA.md TB.md;
    mat_eq "init_matrix" TA.me TB.me;
    mat_eq "init_matrix" TA.mf TB.mf;
    mat_eq "matrix multiply" TA.mg TB.mg;
    mat_eq "matrix multiply_t" TA.mg' TB.mg';
    mat_eq "matrix multiply_nt" TA.mg'' TB.mg'';
    mat_eq "map2f" TA.me TB.me;
    mat_eq "map2f'" TA.me' TB.me';
    mat_eq "from_array2 transposee" TA.mh TB.mh;
    mat_eq "from_array2" TA.mi TB.mi;
    
end
module TestWith (A : Activation.Activation) = struct
  module T1 = Unit (MLArray(A)) (LacamlMat(A))
  module T2 = Unit (MLArray(A)) (CuMat(A))
end

module T1 = TestWith(Activation.Tanh)
module T2 = TestWith(Activation.Sigmoid)
