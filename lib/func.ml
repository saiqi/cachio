let piecewise thresholds values x =
  assert (List.length values = List.length thresholds + 1);
  let rec aux ths vals x =
    match (ths, vals) with
    | [], v :: _ -> v
    | t :: ts, v :: vs -> if x <= t then v else aux ts vs x
    | _ -> failwith "Invalid thresholds or values"
  in
  aux thresholds values x
