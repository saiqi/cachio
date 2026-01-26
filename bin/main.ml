open Cachio

let num_simulations () =
  let usage () =
    prerr_endline "Usage: cachio [num_simulations]";
    exit 1
  in
  match Sys.argv with
  | [| _ |] -> 1
  | [| _; n |] -> (
      match int_of_string_opt n with Some n when n > 0 -> n | _ -> usage ())
  | _ -> usage ()

let run () =
  let n = num_simulations () in
  let module R = Rng.Std in
  let rng = R.create () in
  let sim = Simulation.create (module R) rng Scenario.deterministic_ais in
  let runs = Simulation.run_n n (module R) rng sim in
  let audits = runs |> List.map snd |> List.flatten in
  let stats = Stats.of_audits audits in
  let report = Report.of_stats stats in
  Terminal_report.print report

let () =
  Printexc.record_backtrace true;
  try run ()
  with exn ->
    prerr_endline (Printexc.to_string exn);
    prerr_endline (Printexc.get_backtrace ());
    raise exn
