open Cachio

let run () =
  let module R = Rng.Std in
  let rng = R.create () in
  let sim = Simulation.create (module R) rng Scenario.deterministic_ais in
  let runs = Simulation.run_n 1 (module R) rng sim in
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
