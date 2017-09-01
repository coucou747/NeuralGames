open ArrayAbstraction

module Test (L : LinearOperations) = struct
  
  let a = L.init 7 (fun i -> float_of_int (i * i + 10))
  let b = L.init 7 (fun i -> float_of_int i)
  let c = L.v_times a b
  let d = L.diff a b
  let e = L.map (fun x -> x *. x) a
  let f = L.squaresumdiff a b
  let g = L.init 5 (fun i -> float_of_int (i * i * i))
      
  let ma = L.init_matrix 5 7 (fun x y -> float_of_int (x * 100 + y))
  let mb = L.init_matrix 5 7 (fun x y -> float_of_int (x * 10 + y * y * 15))
  let mc = L.add ma mb

  let h = L.multiply21 ma a
  let i = L.multiply12 g ma

  let md = L.scalar_vects_to_map e b
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
           
  let vec_eq str v1 v2 =
    let v1 = A.to_array v1
    and v2 = B.to_array v2 in
    Format.printf "@[<v10> %S%a =@\n%a@] : %b@\n" str
      pp_vec v1 pp_vec v2
      (v1 = v2);
    assert (v1 = v2)
      
  let mat_eq str v1 v2 =
    let v1 = A.to_array2 v1
    and v2 = B.to_array2 v2 in
    Format.printf "@[<v10>%S %a =@\n%a@] : %b@\n" str
      pp_mat v1 pp_mat v2
      (v1 = v2);
    assert (v1 = v2)
      
  let float_eq str f1 f2 =
    Format.printf "%S %.3f = %.3f : %b@\n" str f1 f2 (f1 = f2);
    assert (f1 = f2)
    
  let () =
    vec_eq "init" TA.a TB.a;
    vec_eq "init" TA.b TB.b;
    vec_eq "times" TA.c TB.c;
    A.scalar TA.a 2.;
    B.scalar TB.a 2.;
    vec_eq "Scalar" TA.a TB.a;
    vec_eq "Map" TA.e TB.e;
    float_eq "squaresumdiff" TA.f TB.f;
    vec_eq "init" TA.g TB.g;
    mat_eq "init_matrix" TA.ma TB.ma;
    mat_eq "init_matrix" TA.mb TB.mb;
    mat_eq "add" TA.mc TB.mc;
    vec_eq "multiply21" TA.h TB.h;
    vec_eq "multiply12" TA.i TB.i;
    mat_eq "init_matrix" TA.md TB.md;
    
end

module T = Unit (MLArray) (LacamlMat)
