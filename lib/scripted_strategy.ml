let offense =
  Round_param.create ~offensive_dice:(Dice_count.of_int_exn 3)
    ~defensive_dice:(Dice_count.of_int_exn 1)
    ~actions:(Action_count.of_int_exn 3)

let defense =
  Round_param.create ~offensive_dice:(Dice_count.of_int_exn 1)
    ~defensive_dice:(Dice_count.of_int_exn 3)
    ~actions:(Action_count.of_int_exn 3)

let balanced =
  Round_param.create ~offensive_dice:(Dice_count.of_int_exn 2)
    ~defensive_dice:(Dice_count.of_int_exn 2)
    ~actions:(Action_count.of_int_exn 3)

let of_id i =
  match i with
  | Strategy_id.Scripted_Balanced -> balanced
  | Strategy_id.Scripted_Offensive -> offense
  | Strategy_id.Scripted_Defensive -> defense
  | _ -> invalid_arg "Not a scripted strategy"
