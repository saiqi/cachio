module type S = sig
  type t

  val int : t -> int -> int
end

module Std : S = struct
  type t = Random.State.t

  let int = Random.State.int
end
