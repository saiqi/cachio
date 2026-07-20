type scenario = Deterministic | Scripted

type t = {
  name : string;
  simulations : int;
  seed : int;
  scenario : scenario;
  created_at : string;
  git_commit : string;
  report : Report.t;
}

let deterministic_ais =
  [
    (Ai_id.of_int 0, Strategy_id.Dummy);
    (Ai_id.of_int 1, Strategy_id.Defensive);
    (Ai_id.of_int 2, Strategy_id.Offensive);
    (Ai_id.of_int 3, Strategy_id.Balanced);
    (Ai_id.of_int 4, Strategy_id.Pragmatic);
    (Ai_id.of_int 5, Strategy_id.Optimal);
  ]

let scripted_ais =
  [
    (Ai_id.of_int 0, Strategy_id.Optimal);
    (Ai_id.of_int 1, Strategy_id.Optimal);
    (Ai_id.of_int 2, Strategy_id.Optimal);
    (Ai_id.of_int 3, Strategy_id.Scripted_Balanced);
    (Ai_id.of_int 4, Strategy_id.Scripted_Defensive);
    (Ai_id.of_int 5, Strategy_id.Scripted_Offensive);
  ]

let string_of_scenario = function
  | Deterministic -> "deterministic"
  | Scripted -> "scripted"

let scenario_of_string = function
  | "deterministic" -> Some Deterministic
  | "scripted" -> Some Scripted
  | _ -> None

let ais_of_scenario = function
  | Deterministic -> deterministic_ais
  | Scripted -> scripted_ais

let created_at () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let read_first_line command =
  let input = Unix.open_process_in command in
  let line = try input_line input with End_of_file -> "unknown" in
  let _ = Unix.close_process_in input in
  if String.length line = 0 then "unknown" else line

let git_commit () = read_first_line "git rev-parse --short HEAD 2>/dev/null"

let create ~name ~simulations ~seed ~scenario =
  let module R = Rng.Std in
  let rng = R.create_seeded seed in
  let runs =
    Simulation.run_n simulations (module R) rng (ais_of_scenario scenario)
  in
  let audits = runs |> List.map snd |> List.flatten in
  let stats = Stats.of_audits audits in
  let report = Report.of_stats stats in
  {
    name;
    simulations;
    seed;
    scenario;
    created_at = created_at ();
    git_commit = git_commit ();
    report;
  }

let report audit = audit.report

let metadata_to_yojson audit =
  `Assoc
    [
      ("name", `String audit.name);
      ("seed", `Int audit.seed);
      ("num_simulations", `Int audit.simulations);
      ("scenario", `String (string_of_scenario audit.scenario));
      ("created_at", `String audit.created_at);
      ("git_commit", `String audit.git_commit);
    ]

let to_yojson audit =
  `Assoc
    [
      ("metadata", metadata_to_yojson audit);
      ("report", Report.to_yojson audit.report);
    ]

let assoc key fields =
  match List.assoc_opt key fields with
  | Some v -> Ok v
  | None -> Error ("missing field: " ^ key)

let string_field key fields =
  match assoc key fields with
  | Ok (`String s) -> Ok s
  | Ok _ -> Error ("invalid string field: " ^ key)
  | Error e -> Error e

let int_field key fields =
  match assoc key fields with
  | Ok (`Int i) -> Ok i
  | Ok _ -> Error ("invalid int field: " ^ key)
  | Error e -> Error e

let metadata_of_yojson = function
  | `Assoc fields -> (
      match
        ( string_field "name" fields,
          int_field "num_simulations" fields,
          int_field "seed" fields,
          string_field "scenario" fields,
          string_field "created_at" fields,
          string_field "git_commit" fields )
      with
      | ( Ok name,
          Ok simulations,
          Ok seed,
          Ok scenario,
          Ok created_at,
          Ok git_commit ) -> (
          match scenario_of_string scenario with
          | Some scenario ->
              Ok (name, simulations, seed, scenario, created_at, git_commit)
          | None -> Error ("unknown scenario: " ^ scenario))
      | Error e, _, _, _, _, _
      | _, Error e, _, _, _, _
      | _, _, Error e, _, _, _
      | _, _, _, Error e, _, _
      | _, _, _, _, Error e, _
      | _, _, _, _, _, Error e ->
          Error e)
  | _ -> Error "invalid metadata"

let of_yojson = function
  | `Assoc fields -> (
      match (assoc "metadata" fields, assoc "report" fields) with
      | Ok metadata_json, Ok report_json -> (
          match
            (metadata_of_yojson metadata_json, Report.of_yojson report_json)
          with
          | ( Ok (name, simulations, seed, scenario, created_at, git_commit),
              Ok report ) ->
              Ok
                {
                  name;
                  simulations;
                  seed;
                  scenario;
                  created_at;
                  git_commit;
                  report;
                }
          | Error e, _ | _, Error e -> Error e)
      | Error e, _ | _, Error e -> Error e)
  | _ -> Error "invalid audit"

let rec mkdir_p dir =
  if dir = "" || dir = "." || Sys.file_exists dir then ()
  else (
    mkdir_p (Filename.dirname dir);
    Unix.mkdir dir 0o755)

let slug s =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c -> c | _ -> '-')
    s

let file_prefix audit =
  let timestamp =
    audit.created_at |> String.map (function ':' -> '-' | c -> c)
  in
  Printf.sprintf "%s_%s_seed-%d_n-%d" timestamp (slug audit.name) audit.seed
    audit.simulations

let write_file path content =
  let output = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out output)
    (fun () -> output_string output content)

let write ~out_dir audit =
  mkdir_p out_dir;
  let prefix = file_prefix audit in
  let txt_path = Filename.concat out_dir (prefix ^ ".txt") in
  let json_path = Filename.concat out_dir (prefix ^ ".json") in
  write_file txt_path (Terminal_report.to_string audit.report);
  write_file json_path (Yojson.Safe.pretty_to_string (to_yojson audit));
  (txt_path, json_path)

type comparable_value =
  | Scalar of { value : float; suffix : string; percent : bool }
  | Rendered of string

let rec rendered_value = function
  | Report.Float f -> Printf.sprintf "%.3f" f
  | Report.Percent p -> Printf.sprintf "%.1f%%" p
  | Report.Int i -> string_of_int i
  | Report.Interval (l, r) -> rendered_value l ^ "-" ^ rendered_value r
  | Report.Optional None -> "n/a"
  | Report.Optional (Some v) -> rendered_value v

let rec comparable_value = function
  | Report.Float f -> Some (Scalar { value = f; suffix = ""; percent = false })
  | Report.Percent p ->
      Some (Scalar { value = p; suffix = " pts"; percent = true })
  | Report.Int i ->
      Some (Scalar { value = float_of_int i; suffix = ""; percent = false })
  | Report.Interval _ as value -> Some (Rendered (rendered_value value))
  | Report.Optional (Some v) -> comparable_value v
  | Report.Optional None -> None

let metric_path section metric =
  Report.title section ^ " / " ^ Report.name metric

let report_values report =
  report |> Report.to_list
  |> List.fold_left
       (fun acc section ->
         section |> Report.metrics
         |> List.fold_left
              (fun acc metric ->
                match comparable_value (Report.value metric) with
                | Some value -> (metric_path section metric, value) :: acc
                | None -> acc)
              acc)
       []

let priority_paths =
  [
    "Global / Win / initial draw dependency";
    "Global / Worst initial draw win ratio";
    "Global / Best initial draw win ratio";
    "Global / Initial draw win-rate spread";
    "Global / Goal per game (mean)";
    "Home advantage / Win ratio";
  ]

let priority_path path =
  List.mem path priority_paths
  || String.ends_with ~suffix:" / Win rate CI" path
  || String.ends_with ~suffix:" / Goals per action" path

let format_scalar percent value =
  if percent then Printf.sprintf "%.1f%%" value else Printf.sprintf "%.3f" value

let format_delta percent suffix value =
  if percent then Printf.sprintf "%+.1f%s" value suffix
  else Printf.sprintf "%+.3f%s" value suffix

let compare ~baseline ~candidate =
  let baseline_values = report_values baseline.report in
  let candidate_values = report_values candidate.report in
  let lines =
    candidate_values
    |> List.filter_map (fun (path, candidate_value) ->
        if not (priority_path path) then None
        else
          match List.assoc_opt path baseline_values with
          | None -> None
          | Some baseline_value -> (
              match (baseline_value, candidate_value) with
              | ( Scalar { value = baseline_value; suffix; percent },
                  Scalar { value = candidate_value; _ } ) ->
                  let delta = candidate_value -. baseline_value in
                  Some
                    ( path,
                      Printf.sprintf "%-45s %s -> %s (%s)" path
                        (format_scalar percent baseline_value)
                        (format_scalar percent candidate_value)
                        (format_delta percent suffix delta) )
              | Rendered baseline_value, Rendered candidate_value ->
                  Some
                    ( path,
                      Printf.sprintf "%-45s %s -> %s" path baseline_value
                        candidate_value )
              | _ -> None))
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map snd
  in
  String.concat "\n" lines ^ "\n"
