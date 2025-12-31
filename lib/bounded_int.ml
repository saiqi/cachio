module type BOUNDS = sig
  val min : int
  val max : int
end

module type S = sig
  type t

  val min : int
  val max : int
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val incr : t -> t
  val decr : t -> t
  val add : t -> t -> int
  val compare : t -> t -> int
end

module Make (B : BOUNDS) : S = struct
  type t = int

  let min = B.min
  let max = B.max
  let of_int x = if x >= min && x <= max then Some x else None

  let of_int_exn x =
    match of_int x with Some v -> v | None -> invalid_arg "Dice.of_int"

  let to_int (x : t) = x
  let clamp x = if x < min then min else if x > max then max else x
  let incr x = clamp (x + 1)
  let decr x = clamp (x - 1)
  let add x y = to_int x + y
  let compare = Int.compare
end
