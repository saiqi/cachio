module type S = sig
  type t

  val create : unit -> t
  val int : t -> int -> int
end

module Std : S
