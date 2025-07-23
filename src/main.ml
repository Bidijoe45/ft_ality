
let parse_file (in_chan : in_channel) : Lexer.production_rule list =
  let input_line_opt ic = try Some (input_line ic) with End_of_file -> None in
  let rec parse_lines acc =
    match (input_line_opt in_chan) with
    | None -> acc
    | Some line -> match (Lexer.tokenize_rule line) with
      | None -> parse_lines acc
      | Some r -> parse_lines (acc @ [r])
  in parse_lines []

let () =
  let grammar_file_path = match Sys.argv with
    | [| |] | [| _ |] -> print_endline "Too few arguments.\nUsage: ft_ality <path_to_grammar_file>"; exit 1
    | [| _ ; path |] -> path
    | _ -> print_endline "Too many arguments.\nUsage: ft_ality <path_to_grammar_file>"; exit 1
  in
  let in_channel =
    try open_in grammar_file_path
    with Sys_error msg -> print_endline ("Error while opening file for reading: " ^ msg); exit 1 in
  let production_rules = parse_file in_channel in
  (* List.iter (fun rule -> print_endline (Lexer.rule_to_string rule)) production_rules; *)
  let trie = Automaton.train production_rules in
  (* print_endline (Automaton.trie_to_string trie); *)
  let read_line_opt () = try Some (read_line ()) with End_of_file -> None in
  let rec read_input () = match read_line_opt () with
    | None -> ()
    | Some input ->
        let result, _ = Automaton.run trie input 0 in
        if (Option.is_none result) then print_endline "Unrecognised combo.";
        read_input ()
  in read_input ()
