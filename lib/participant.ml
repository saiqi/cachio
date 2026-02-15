type t = Ai of Ai.t | Scripted of Scripted.t

let of_ai ai = Ai ai
let of_scripted s = Scripted s
let id = function Ai ai -> Ai.id ai | Scripted s -> Scripted.id s

let strategy = function
  | Ai ai -> Ai.strategy ai
  | Scripted s -> Scripted.strategy s

let roster = function Ai ai -> Some (Ai.roster ai) | Scripted _ -> None

let after_game result = function
  | Ai ai ->
      let players =
        if Ai_id.equal (Ai.id ai) (Game_result.home result) then
          Game_result.home_players result
        else Game_result.away_players result
      in
      let ai' =
        Ai.with_roster (Fatigue.apply ~players ~roster:(Ai.roster ai)) ai
      in
      Ai ai'
  | Scripted s -> Scripted s

let reveal (type a) (module R : Rng.S with type t = a) (rng : a) ~home =
  function
  | Ai ai ->
      let board = Ai.build_board (module R) rng ai home in
      let param = Round.compute_param ~home ~board ~roster:(Ai.roster ai) in
      (param, Some board)
  | Scripted s -> (Scripted.param s, None)
