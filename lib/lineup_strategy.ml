type t = { id : Strategy_id.t; eval : Board.t -> Roster.t -> int }

let offense = { eval = Lineup_eval.offense_score; id = Strategy_id.Offensive }
let defense = { eval = Lineup_eval.defense_score; id = Strategy_id.Defensive }
let balanced = { eval = Lineup_eval.balanced_score; id = Strategy_id.Balanced }

let optimal home =
  { eval = Lineup_eval.optimal_score home; id = Strategy_id.Optimal }

let pragmatic home =
  { eval = Lineup_eval.pragmatic_score home; id = Strategy_id.Pragmatic }

let make id f = { eval = f; id }

let build ?(generate = Lineup_generator.all) strategy roster =
  let boards = generate roster in
  match boards with
  | [] -> invalid_arg "Lineup_strategy.build: empty lineup set"
  | b0 :: bs ->
      let score0 = strategy.eval b0 roster in
      let _, best =
        List.fold_left
          (fun (best_score, best_board) b ->
            let s = strategy.eval b roster in
            if s > best_score then (s, b) else (best_score, best_board))
          (score0, b0) bs
      in
      best

let id x = x.id

let of_id ~home = function
  | Strategy_id.Defensive -> defense
  | Strategy_id.Balanced -> balanced
  | Strategy_id.Offensive -> offense
  | Strategy_id.Optimal -> optimal home
  | Strategy_id.Pragmatic -> pragmatic home
  | _ -> invalid_arg "Not Implemented"
