type scenario = Deterministic | Scripted
type t

val scenario_of_string : string -> scenario option
val string_of_scenario : scenario -> string
val ais_of_scenario : scenario -> (Ai_id.t * Strategy_id.t) list

val create :
  name:string -> simulations:int -> seed:int -> scenario:scenario -> t

val report : t -> Report.t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val write : out_dir:string -> t -> string * string
val compare : baseline:t -> candidate:t -> string
