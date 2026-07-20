module type S = sig
  type t

  val create : unit -> t
  val create_seeded : int -> t
  val int : t -> int -> int
end

module Std : S = struct
  type t = Random.State.t

  let create () = Random.State.make_self_init ()
  let create_seeded seed = Random.State.make [| seed |]
  let int = Random.State.int
end
