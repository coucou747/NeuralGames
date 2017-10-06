let fold1 f li =
  match li with
  | hd::tl -> List.fold_left f hd tl
  | _ -> assert false
    

let rec seq_append a b li =
  if a > b then li
  else seq_append a (b - 1) (b :: li)

let (--) a b = seq_append a b []

let pp_option pp f opt =
  match opt with
  | None -> Format.fprintf f "None"
  | Some x -> Format.fprintf f "Some(%a)" pp x

let shuffle li =
  let li = Array.of_list li in
  let len = Array.length li in
  for i = 0 to len - 1 do
    let n = i + Random.int (len - i) in
    let tmp = li.(i) in
    li.(i) <- li.(n);
    li.(n) <- tmp
  done;
  Array.to_list li

let fprintf_ error fmt =
  match error with
  | Some f -> Format.fprintf f fmt
  | None -> Format.ifprintf Format.std_formatter fmt
