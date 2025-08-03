let () =

  let grammar_file_path, debug_mode = match Sys.argv with
    | [| |] | [| _ |] -> print_endline "Too few arguments.\nUsage: ft_ality <path_to_grammar_file> [ debug ]"; exit 1
    | [| _ ; path |] -> path, false
    | [| _ ; path ; debug |] ->
      if debug <> "debug"
      then (print_endline "Invalid argument.\nUsage: ft_ality <path_to_grammar_file> [ debug ]"; exit 1)
      else (path, true)
    | _ -> print_endline "Too many arguments.\nUsage: ft_ality <path_to_grammar_file> [ debug ]"; exit 1
  in

  let in_channel =
    try open_in grammar_file_path
    with Sys_error msg -> print_endline ("Error while opening file for reading: " ^ msg); exit 1 in

  let tokens = Grammar.Lexer.tokenize in_channel in

  match Grammar.Parser.parse tokens with
  | None -> print_endline "Invalid file"; exit 1
  | Some (key_mappings, production_rules) ->
    List.iter (fun x -> print_endline (Grammar.key_mapping_to_string x)) key_mappings;
    if debug_mode then List.iter (fun x -> print_endline (Grammar.rule_to_string x)) production_rules;
    let trie = Automaton.train production_rules in
    Automaton.run key_mappings trie debug_mode
