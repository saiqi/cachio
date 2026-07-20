open Cachio

let usage () =
  prerr_endline
    "Usage:\n\
    \  cachio run [num_simulations]\n\
    \  cachio audit --name NAME --simulations N [--seed SEED] [--scenario \
     deterministic|scripted] [--out DIR]\n\
    \  cachio compare BASELINE_JSON CANDIDATE_JSON";
  exit 1

let positive_int value =
  match int_of_string_opt value with Some n when n > 0 -> n | _ -> usage ()

let run simulations =
  let module R = Rng.Std in
  let rng = R.create () in
  let runs =
    Simulation.run_n simulations
      (module R)
      rng
      (Balance_audit.ais_of_scenario Balance_audit.Deterministic)
  in
  let stats = Stats.of_runs runs in
  let report = Report.of_stats stats in
  Terminal_report.print report

let parse_run = function
  | [||] -> run 1
  | [| n |] -> run (positive_int n)
  | _ -> usage ()

let parse_audit argv =
  let name = ref None in
  let simulations = ref None in
  let seed = ref 0 in
  let scenario = ref Balance_audit.Deterministic in
  let out_dir = ref "reports/balance" in
  let set_scenario value =
    match Balance_audit.scenario_of_string value with
    | Some value -> scenario := value
    | None -> usage ()
  in
  let spec =
    [
      ("--name", Arg.String (fun value -> name := Some value), "Audit name");
      ( "--simulations",
        Arg.String (fun value -> simulations := Some (positive_int value)),
        "Number of simulations" );
      ("--seed", Arg.Int (fun value -> seed := value), "Random seed");
      ( "--scenario",
        Arg.String set_scenario,
        "Scenario: deterministic or scripted" );
      ("--out", Arg.String (fun value -> out_dir := value), "Output directory");
    ]
  in
  let anon _ = usage () in
  Arg.parse_argv ~current:(ref 0)
    (Array.append [| "cachio audit" |] argv)
    spec anon "Usage: cachio audit --name NAME --simulations N";
  match (!name, !simulations) with
  | Some name, Some simulations ->
      let audit =
        Balance_audit.create ~name ~simulations ~seed:!seed ~scenario:!scenario
      in
      let txt_path, json_path = Balance_audit.write ~out_dir:!out_dir audit in
      Printf.printf "Wrote %s\nWrote %s\n" txt_path json_path
  | _ -> usage ()

let load_audit path =
  match Balance_audit.of_yojson (Yojson.Safe.from_file path) with
  | Ok audit -> audit
  | Error e ->
      prerr_endline ("Invalid audit file " ^ path ^ ": " ^ e);
      exit 1

let parse_compare = function
  | [| baseline_path; candidate_path |] ->
      let baseline = load_audit baseline_path in
      let candidate = load_audit candidate_path in
      print_string (Balance_audit.compare ~baseline ~candidate)
  | _ -> usage ()

let dispatch () =
  match Sys.argv with
  | [| _ |] -> parse_run [||]
  | [| _; n |] when int_of_string_opt n <> None -> parse_run [| n |]
  | argv -> (
      let command = argv.(1) in
      let rest = Array.sub argv 2 (Array.length argv - 2) in
      match command with
      | "run" -> parse_run rest
      | "audit" -> parse_audit rest
      | "compare" -> parse_compare rest
      | _ -> usage ())

let () =
  Printexc.record_backtrace true;
  try dispatch ()
  with exn ->
    prerr_endline (Printexc.to_string exn);
    prerr_endline (Printexc.get_backtrace ());
    raise exn
