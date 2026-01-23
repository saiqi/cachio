type t = int list ref

let create () = ref []

let int rng bound =
  match !rng with
  | x :: xs ->
      rng := xs;
      x mod bound
  | [] -> 0
