let offensive_dice_curve =
  Piecewise.of_list [ (0, 1); (6, 2); (8, 3); (10, 4); (12, 5) ]

let defensive_dice_curve = offensive_dice_curve
let action_curve = Piecewise.of_list [ (0, 1); (4, 2); (6, 3); (8, 4); (10, 5) ]
