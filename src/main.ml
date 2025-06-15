
let parse_file (in_chan : in_channel) =
  let input_line_opt ic = try Some (input_line ic) with End_of_file -> None in
  let rec parse_lines acc =
    let line = input_line_opt in_chan in
    match line with
    | None -> acc
    | Some l -> parse_lines acc @ [(Lexer.tokenize_rule l)]
  in parse_lines []

let () =
  let in_channel = open_in "grammar/example.gmr" in
  let production_rules = parse_file in_channel in
  List.iter (fun rule -> print_endline (Lexer.rule_to_string rule)) production_rules
