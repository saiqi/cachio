module type S = sig
  type t

  val int : t -> int -> int
end

module Std : S
