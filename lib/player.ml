type position = DEFENDER | MIDFIELD | FORWARD
type t = { id : Player_id.t; pos : position; shape : Shape.t; score : Score.t }

let string_of_position x =
  match x with
  | DEFENDER -> "defender"
  | MIDFIELD -> "midfield"
  | FORWARD -> "forward"

let create id pos score = { id; pos; shape = Shape.of_int_exn Shape.max; score }
let id x = x.id
let pos x = x.pos
let shape x = x.shape
let score x = x.score

let incr_shape x =
  { id = x.id; pos = x.pos; shape = Shape.incr x.shape; score = x.score }

let decr_shape x =
  { id = x.id; pos = x.pos; shape = Shape.decr x.shape; score = x.score }

let incr_score x =
  { id = x.id; pos = x.pos; shape = x.shape; score = Score.incr x.score }

let decr_score x =
  { id = x.id; pos = x.pos; shape = x.shape; score = Score.decr x.score }

let is_injured x = Shape.to_int x.shape == Shape.min

let adjust_score x pos =
  let score =
    match x.pos with
    | DEFENDER -> (
        match pos with
        | DEFENDER -> x.score
        | MIDFIELD -> Score.decr x.score
        | FORWARD -> Score.decr (Score.decr x.score))
    | MIDFIELD -> (
        match pos with
        | DEFENDER -> Score.decr x.score
        | MIDFIELD -> x.score
        | FORWARD -> Score.decr x.score)
    | FORWARD -> (
        match pos with
        | DEFENDER -> Score.decr (Score.decr x.score)
        | MIDFIELD -> Score.decr x.score
        | FORWARD -> x.score)
  in
  if is_injured x then Score.of_int_exn Score.min else score
