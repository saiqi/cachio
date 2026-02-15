type t

val create : Ai_id.t -> Strategy_id.t -> t
val id : t -> Ai_id.t
val strategy : t -> Strategy_id.t
val param : t -> Round_param.t
