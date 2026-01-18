type t = {
  id : Player_id.t;
  pos : Position.t;
  shape : Shape.t;
  score : Score.t;
}

let create id pos score = { id; pos; shape = Shape.max; score }
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

let equal p1 p2 =
  p1.id = p2.id && p1.pos = p2.pos && p1.shape = p2.shape && p1.score = p2.score

let is_injured p = Shape.equal (shape p) Shape.min

let adjust_score p position =
  if is_injured p then
    { id = id p; pos = pos p; shape = shape p; score = Score.min }
  else
    match pos p with
    | Position.Defender -> (
        match position with
        | Position.Defender -> p
        | Position.Midfielder -> decr_score p
        | Position.Forward -> decr_score (decr_score p))
    | Position.Midfielder -> (
        match position with
        | Position.Defender -> decr_score p
        | Position.Midfielder -> p
        | Position.Forward -> decr_score (decr_score p))
    | Position.Forward -> (
        match position with
        | Position.Defender -> decr_score (decr_score p)
        | Position.Midfielder -> decr_score p
        | Position.Forward -> p)
