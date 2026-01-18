module type BOUNDS = sig
  val min : int
  val max : int
end

module type S = sig
  type t

  val min : t
  val max : t
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val incr : t -> t
  val decr : t -> t
  val add : t -> t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val all : t list
end

module Make (_ : BOUNDS) : S
