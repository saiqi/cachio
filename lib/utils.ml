let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let rec drop n = function
  | [] -> []
  | xs when n <= 0 -> xs
  | _ :: xs -> drop (n - 1) xs

let draw n l = (drop n l, take n l)

let shuffle (type a) (module R : Rng.S with type t = a) (rng : a) l =
  let arr = Array.of_list l in
  for i = Array.length arr - 1 downto 1 do
    let j = R.int rng (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr
