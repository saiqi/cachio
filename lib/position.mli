type t = Defender | Midfielder | Forward

val all_positions : t list
val string_of_position : t -> string
val compare : t -> t -> int
