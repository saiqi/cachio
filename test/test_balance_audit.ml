open Cachio

let make_audit_json win_ratio =
  `Assoc
    [
      ( "metadata",
        `Assoc
          [
            ("name", `String "test");
            ("seed", `Int 0);
            ("num_simulations", `Int 1);
            ("scenario", `String "deterministic");
            ("created_at", `String "2026-07-20T00:00:00Z");
            ("git_commit", `String "test");
          ] );
      ( "report",
        `List
          [
            `Assoc
              [
                ("title", `String "Global");
                ( "metrics",
                  `List
                    [
                      `Assoc
                        [
                          ("name", `String "Worst initial draw win ratio");
                          ( "value",
                            `Assoc
                              [
                                ("type", `String "percent");
                                ("value", `Float win_ratio);
                              ] );
                        ];
                    ] );
              ];
            `Assoc
              [
                ("title", `String "Offensive");
                ( "metrics",
                  `List
                    [
                      `Assoc
                        [
                          ("name", `String "Win rate CI");
                          ( "value",
                            `Assoc
                              [
                                ("type", `String "interval");
                                ( "lower",
                                  `Assoc
                                    [
                                      ("type", `String "float");
                                      ("value", `Float 0.3);
                                    ] );
                                ( "upper",
                                  `Assoc
                                    [
                                      ("type", `String "float");
                                      ("value", `Float 0.4);
                                    ] );
                              ] );
                        ];
                    ] );
              ];
          ] );
    ]

let test_compare () =
  let baseline =
    match Balance_audit.of_yojson (make_audit_json 26.2) with
    | Ok audit -> audit
    | Error e -> Alcotest.fail e
  in
  let candidate =
    match Balance_audit.of_yojson (make_audit_json 31.0) with
    | Ok audit -> audit
    | Error e -> Alcotest.fail e
  in
  let output = Balance_audit.compare ~baseline ~candidate in
  Alcotest.check Alcotest.bool "contains metric" true
    (String.contains output 'W');
  Alcotest.check Alcotest.bool "contains delta" true
    (String.contains output '+');
  Alcotest.check Alcotest.bool "contains interval" true
    (String.contains output '-')

let test_scenario_parser () =
  Alcotest.check Alcotest.bool "deterministic" true
    (Balance_audit.scenario_of_string "deterministic"
    = Some Balance_audit.Deterministic);
  Alcotest.check Alcotest.bool "scripted" true
    (Balance_audit.scenario_of_string "scripted" = Some Balance_audit.Scripted);
  Alcotest.check Alcotest.bool "unknown" true
    (Balance_audit.scenario_of_string "unknown" = None)

let suite =
  [
    ("compare", `Quick, test_compare);
    ("scenario parser", `Quick, test_scenario_parser);
  ]
