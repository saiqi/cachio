type t = {
  id : Strategy_id.t;
  eval : Board.t -> Roster.t -> int;
  n_iterations : int;
}

let offense =
  {
    eval = Lineup_eval.offense_score;
    id = Strategy_id.Offensive;
    n_iterations = 1000;
  }

let defense =
  {
    eval = Lineup_eval.defense_score;
    id = Strategy_id.Defensive;
    n_iterations = 1000;
  }

let balanced =
  {
    eval = Lineup_eval.balanced_score;
    id = Strategy_id.Balanced;
    n_iterations = 1000;
  }

let optimal home =
  {
    eval = Lineup_eval.optimal_score home;
    id = Strategy_id.Optimal;
    n_iterations = 2000;
  }

let pragmatic home =
  {
    eval = Lineup_eval.pragmatic_score home;
    id = Strategy_id.Pragmatic;
    n_iterations = 1000;
  }

let dummy =
  { eval = Lineup_eval.dummy_score; id = Strategy_id.Dummy; n_iterations = 10 }

let make id f n_iterations = { eval = f; id; n_iterations }

let build (type a) (module R : Rng.S with type t = a) (rng : a)
    ?(generate = Lineup_generator.all) strategy roster =
  let boards =
    generate (module R) rng roster
    |> Seq.take strategy.n_iterations
    |> List.of_seq
  in
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
  | Strategy_id.Dummy -> dummy
