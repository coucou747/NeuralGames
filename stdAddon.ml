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
