let rec string_of_value = function
  | Report.Float f -> Printf.sprintf "%.3f" f
  | Report.Percent p -> Printf.sprintf "%.1f%%" p
  | Report.Optional None -> "n/a"
  | Report.Optional (Some v) -> string_of_value v
  | Report.Int i -> Printf.sprintf "%d" i

let print_metric metric =
  Printf.printf "  %-35s %s\n" (Report.name metric)
    (string_of_value (Report.value metric))

let print_section section =
  Printf.printf "\n%s\n" (Report.title section);
  Printf.printf "%s\n" (String.make (String.length (Report.title section)) '-');
  List.iter print_metric (Report.metrics section)

let print report = List.iter print_section (Report.to_list report)
