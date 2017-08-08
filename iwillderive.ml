
type op = Add | Mul

type 'a func =
  | X
  | Cst of float
  | Binop of 'a * op * 'a
  | Inverse of 'a

type fixed = {s : fixed func}

let rec fold f v = match v.s with
  | X -> f X
  | Cst v -> f (Cst v)
  | Binop (a, op, b) -> f (Binop (fold f a, op, fold f b))
  | Inverse a -> f (Inverse (fold f a))

let cst f = {s=Cst f}
let ( ** ) a b = {s=Binop (a, Mul, b)}
let ( *. ) a b = {s=Binop (a, Mul, cst b)}
let (++) a b = {s=Binop (a, Add, b)}
let (//) a b = a ** {s=Inverse b}
let (--) a b = a ++ b *. -1.

let x = {s=X}

let derivate = function
  | X -> (cst 1., x)
  | Cst x -> (cst 0., cst x)
  | Binop ( (da, a), Add, (db, b)) ->
    (da ++ db, a ++ b)
  | Binop ( (da, a), Mul, (db, b)) ->
    (da ** b ++ db ** a, a ** b)
  | Inverse (da, a) ->
    cst 0. -- da // (a ** a), {s=Inverse a}

let derivate x = fst (fold derivate x)

let parens parent me f format =
  if parent > me then Format.fprintf f ("(" ^^ format^^ ")")
  else Format.fprintf f format

let pp = function
    | X -> fun f _top_parens -> Format.fprintf f "x"
    | Cst x -> fun f _top_parens -> Format.fprintf f "%f" x
    | Binop (a, Add, b) -> fun f top_parens -> parens top_parens 0 f "%a + %a" a 0 b 0
    | Binop (a, Mul, b) -> fun f top_parens -> parens top_parens 2 f "%a * %a" a 2 b 2
    | Inverse a -> fun f top_parens -> parens top_parens 1 f "1 / %a" a 1

let pp f x = fold pp x f 0


let func = x ** x ** x

let () = Format.printf "func=%a@\ndfunc=%a@\n" pp func pp (derivate func)
