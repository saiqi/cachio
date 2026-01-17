type game = { home : Ai_id.t; away : Ai_id.t }
type t = game list list

let rotate = function ([] | [ _ ]) as l -> l | x :: xs -> xs @ [ x ]

let pair_round ~round fixed rest =
  let rev = List.rev rest in
  let half = List.length rest / 2 in
  let fixed_pair =
    let opp = List.hd rev in
    if round mod 2 = 0 then { home = fixed; away = opp }
    else { home = opp; away = fixed }
  in
  let left = Utils.take half rest in
  let right = Utils.take half (List.tl rev) in
  fixed_pair
  :: List.map2
       (fun a b ->
         if round mod 2 = 0 then { home = a; away = b }
         else { home = b; away = a })
       left right

let round_robin ids =
  match ids with
  | [] | [ _ ] -> []
  | fixed :: rest ->
      let n = List.length rest in
      let rec aux acc rest round =
        if round = n then List.rev acc
        else
          let r = pair_round ~round fixed rest in
          aux (r :: acc) (rotate rest) (round + 1)
      in
      let first_leg = aux [] rest 0 in
      let second_leg =
        List.map
          (List.map (fun g -> { home = g.away; away = g.home }))
          first_leg
      in
      first_leg @ second_leg

let day = List.nth
let home g = g.home
let away g = g.away
let length = List.length
let to_list x = x
