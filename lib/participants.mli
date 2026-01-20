type t

val empty : t
val add : Ai.t -> t -> t
val find_opt : Ai_id.t -> t -> Ai.t option
val find : Ai_id.t -> t -> Ai.t
val to_list : t -> Ai.t list
val of_list : Ai.t list -> t
