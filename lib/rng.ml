module type S = sig
  type t

  val create : unit -> t
  val int : t -> int -> int
end

module Std : S = struct
  type t = Random.State.t

  let create () = Random.State.make_self_init ()
  let int = Random.State.int
end
