let take n l =
  let rec aux n l =
    if n = 0 then []
    else
      match l with
      | [] -> failwith "empty list"
      | x :: xs -> x :: aux (n - 1) xs
  in
  aux n l
