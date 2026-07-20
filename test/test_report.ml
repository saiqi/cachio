open Cachio

let sample_json =
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
                    ("name", `String "Win ratio");
                    ( "value",
                      `Assoc
                        [ ("type", `String "percent"); ("value", `Float 34.7) ]
                    );
                  ];
                `Assoc
                  [
                    ("name", `String "Total games");
                    ( "value",
                      `Assoc [ ("type", `String "int"); ("value", `Int 30000) ]
                    );
                  ];
                `Assoc
                  [
                    ("name", `String "Missing");
                    ( "value",
                      `Assoc [ ("type", `String "optional"); ("value", `Null) ]
                    );
                  ];
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
                                ("type", `String "float"); ("value", `Float 0.3);
                              ] );
                          ( "upper",
                            `Assoc
                              [
                                ("type", `String "float"); ("value", `Float 0.4);
                              ] );
                        ] );
                  ];
              ] );
        ];
    ]

let test_json_roundtrip () =
  let decoded =
    match Report.of_yojson sample_json with
    | Ok report -> report
    | Error e -> Alcotest.fail e
  in
  match Report.of_yojson (Report.to_yojson decoded) with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
      Alcotest.check Alcotest.int "section count" 1
        (List.length (Report.to_list decoded));
      let section = List.hd (Report.to_list decoded) in
      Alcotest.check Alcotest.string "section title" "Global"
        (Report.title section);
      Alcotest.check Alcotest.int "metric count" 4
        (List.length (Report.metrics section))

let suite = [ ("json roundtrip", `Quick, test_json_roundtrip) ]
