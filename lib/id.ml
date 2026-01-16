module type S = sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Make () : S = struct
  type t = int

  let of_int x = x
  let to_int x = x
  let compare = Int.compare
  let equal = Int.equal
end
