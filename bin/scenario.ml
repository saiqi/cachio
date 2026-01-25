open Cachio

let deterministic_ais =
  [
    (Ai_id.of_int 0, Strategy_id.Dummy);
    (Ai_id.of_int 1, Strategy_id.Defensive);
    (Ai_id.of_int 2, Strategy_id.Offensive);
    (Ai_id.of_int 3, Strategy_id.Balanced);
    (Ai_id.of_int 4, Strategy_id.Pragmatic);
    (Ai_id.of_int 5, Strategy_id.Optimal);
  ]
