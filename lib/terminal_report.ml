let rec string_of_value = function
  | Report.Float f -> Printf.sprintf "%.3f" f
  | Report.Percent p -> Printf.sprintf "%.1f%%" p
  | Report.Optional None -> "n/a"
  | Report.Optional (Some v) -> string_of_value v
  | Report.Int i -> Printf.sprintf "%d" i
  | Report.Interval (l, r) -> string_of_value l ^ "-" ^ string_of_value r

let print_metric metric =
  Printf.printf "  %-35s %s\n" (Report.name metric)
    (string_of_value (Report.value metric))

let print_section section =
  Printf.printf "\n%s\n" (Report.title section);
  Printf.printf "%s\n" (String.make (String.length (Report.title section)) '-');
  List.iter print_metric (Report.metrics section)

let print report = List.iter print_section (Report.to_list report)

let to_string report =
  let buffer = Buffer.create 1024 in
  let add fmt = Printf.bprintf buffer fmt in
  let add_metric metric =
    add "  %-35s %s\n" (Report.name metric)
      (string_of_value (Report.value metric))
  in
  let add_section section =
    add "\n%s\n" (Report.title section);
    add "%s\n" (String.make (String.length (Report.title section)) '-');
    List.iter add_metric (Report.metrics section)
  in
  List.iter add_section (Report.to_list report);
  Buffer.contents buffer
