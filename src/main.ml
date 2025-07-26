let () =
  let grammar_file_path = match Sys.argv with
    | [| |] | [| _ |] -> print_endline "Too few arguments.\nUsage: ft_ality <path_to_grammar_file>"; exit 1
    | [| _ ; path |] -> path
    | _ -> print_endline "Too many arguments.\nUsage: ft_ality <path_to_grammar_file>"; exit 1
  in
  let in_channel =
    try open_in grammar_file_path
    with Sys_error msg -> print_endline ("Error while opening file for reading: " ^ msg); exit 1 in
  let tokens = Grammar_lexer.tokenize in_channel in
  (* List.iter (fun x -> print_endline (Grammar_lexer.token_to_string x)) tokens; *)
  match Grammar_parser.parse tokens with
  | None -> print_endline "Invalid file"
  | Some (key_mappings, production_rules) ->
    List.iter (fun x -> print_endline (Grammar_parser.key_mapping_to_string x)) key_mappings;
    List.iter (fun x -> print_endline (Grammar_parser.rule_to_string x)) production_rules;
    let trie = Automaton.train production_rules in
    print_endline (Automaton.trie_to_string trie);
    Automaton.run key_mappings trie
