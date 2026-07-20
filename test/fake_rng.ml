type t = int list ref

let create () = ref []
let create_seeded _ = create ()

let int rng bound =
  match !rng with
  | x :: xs ->
      rng := xs;
      x mod bound
  | [] -> 0
