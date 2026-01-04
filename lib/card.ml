type t = { position : Position.t; score : Score.t }

let create position score = { position; score }
let position card = card.position
let score card = card.score
