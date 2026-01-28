type t = {
  home : Ai_id.t;
  home_goals : int;
  away : Ai_id.t;
  away_goals : int;
  home_players : Player_id.t list;
  away_players : Player_id.t list;
}

let create ~home ~home_goals ~away ~away_goals ~home_players ~away_players =
  { home; home_goals; away; away_goals; home_players; away_players }

let home r = r.home
let away r = r.away
let home_goals r = r.home_goals
let away_goals r = r.away_goals
let home_players r = r.home_players
let away_players r = r.away_players

let winner r =
  if home_goals r > away_goals r then Some (home r)
  else if home_goals r < away_goals r then Some (away r)
  else None
