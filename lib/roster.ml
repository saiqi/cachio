module M = Map.Make (Player_id)

type t = Player.t M.t

let empty = M.empty
let add p m = M.add (Player.id p) p m
let find_opt = M.find_opt
let find = M.find
let of_list l = List.fold_left (fun acc p -> add p acc) empty l
let to_list m = M.to_list m |> List.map (fun (_, p) -> p)

let of_cards l =
  List.mapi (fun i e -> (i, e)) l
  |> List.fold_left
       (fun acc (i, e) ->
         let id = Player_id.of_int i in
         let p = Player.create id (Card.position e) (Card.score e) in
         add p acc)
       empty
