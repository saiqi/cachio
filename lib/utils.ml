let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let rec drop n = function
  | [] -> []
  | xs when n <= 0 -> xs
  | _ :: xs -> drop (n - 1) xs

let draw n l = (drop n l, take n l)
