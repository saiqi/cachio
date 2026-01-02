type t

val of_list : (int * int) list -> t
val eval : t -> int -> int
