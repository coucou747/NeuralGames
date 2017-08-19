let fold1 f li =
  match li with
  | hd::tl -> List.fold_left f hd tl
  | _ -> assert false
    
