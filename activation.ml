
let rfloat () = (Random.float 2.) -. 1.
module type Activation = sig
  type t = float -> float
  val f : float -> float
  val f' : float-> float
  val cuda_f : Cudabindings.Vec.t -> Cudabindings.Vec.t
  val rand_float : int -> int -> unit -> float
  val convert01 : float -> float
  val invert : float -> float
  val max : float
  val min : float
  val neutral : float
end

module Sigmoid : Activation = struct
  type t = float -> float
  let f x = 1. /. (1. +. exp (-. x))
  let f' x =
    let fx = f x in
    fx *. (1. -. fx)
  let cuda_f = Cudabindings.Vec.sigmoid
  let rand_float nn n =
    let s =  sqrt (1. /. float_of_int (nn + n)) in
    (fun () -> 4. *. rfloat () *. s)
  let convert01 x = x
  let invert x = 1. -. x
  let min = 0.
  let max = 1.
  let neutral = 0.5
end

module Tanh : Activation = struct
  type t = float -> float
  let f x = tanh x
  let f' x = 1. -. (tanh x) *. (tanh x)
  let cuda_f = Cudabindings.Vec.tanH
  let rand_float nn n = 
    let s =  sqrt (1. /. float_of_int (nn + n)) in
    (fun () -> 4. *. rfloat () *. s)
  let convert01 x = x *. 2. -. 1.
  let invert x = -. x
    let min = -.1.
  let max = 1.
  let neutral = 0.
end
