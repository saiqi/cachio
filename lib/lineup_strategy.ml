type t = { id : Strategy_id.t; eval : Board.t -> Roster.t -> int }

let offense = { eval = Lineup_eval.offense_score; id = Strategy_id.Offensive }
let defense = { eval = Lineup_eval.defense_score; id = Strategy_id.Defensive }
let balanced = { eval = Lineup_eval.balanced_score; id = Strategy_id.Balanced }

let optimal home =
  { eval = Lineup_eval.optimal_score home; id = Strategy_id.Optimal }

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
