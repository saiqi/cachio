type t = {
  id : Player_id.t;
  pos : Position.t;
  shape : Shape.t;
  score : Score.t;
}

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
    | Position.Defender -> (
        match pos with
        | Position.Defender -> x.score
        | Position.Midfielder -> Score.decr x.score
        | Position.Forward -> Score.decr (Score.decr x.score))
    | Position.Midfielder -> (
        match pos with
        | Position.Defender -> Score.decr x.score
        | Position.Midfielder -> x.score
        | Position.Forward -> Score.decr x.score)
    | Position.Forward -> (
        match pos with
        | Position.Defender -> Score.decr (Score.decr x.score)
        | Position.Midfielder -> Score.decr x.score
        | Position.Forward -> x.score)
  in
  if is_injured x then Score.of_int_exn Score.min else score
