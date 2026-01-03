module M = Map.Make (Player_id)

type t = Player.t M.t

let empty = M.empty
let add p m = M.add (Player.id p) p m
let find_opt = M.find_opt
let find = M.find
let of_list l = List.fold_left (fun acc p -> add p acc) empty l
