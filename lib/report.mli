type value =
  | Float of float
  | Percent of float
  | Int of int
  | Interval of (value * value)
  | Optional of value option

type metric
type section
type t

val of_stats : Stats.t -> t
val title : section -> string
val metrics : section -> metric list
val name : metric -> string
val value : metric -> value
val to_list : t -> section list
