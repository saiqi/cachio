module M = Map.Make (Ai_id)

type t = Ai.t M.t

let empty = M.empty
let add ai participants = M.add (Ai.id ai) ai participants
let find_opt = M.find_opt
let find = M.find
let to_list participants = M.to_list participants |> List.map (fun (_, i) -> i)
let of_list l = List.fold_left (fun acc ai -> add ai acc) empty l
