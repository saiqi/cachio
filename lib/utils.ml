let draw n l =
  let rec aux n acc rest =
    if n = 0 then (rest, List.rev acc)
    else
      match rest with
      | [] -> failwith "empty list"
      | x :: xs -> aux (n - 1) (x :: acc) xs
  in
  aux n [] l
