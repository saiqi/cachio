type t

val empty : t
val add : Participant.t -> t -> t
val find_opt : Ai_id.t -> t -> Participant.t option
val find : Ai_id.t -> t -> Participant.t
val to_list : t -> Participant.t list
val of_list : Participant.t list -> t
