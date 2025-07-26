type key = char
type symbol = Quit | Reset | Symbol of string
type combo_name = string

type key_mapping = (key * symbol)
type production_rule = (string list * combo_name)

let symbol_to_string (symbol : symbol) : string =
  match symbol with | Quit -> "QUIT" | Reset -> "RESET" | Symbol s -> s

let key_mapping_to_string (key_mapping : key_mapping) : string =
  let (key, symbol) = key_mapping in (String.make 1 key) ^ " -> " ^ (symbol_to_string symbol)

let rule_to_string (rule : production_rule) : string =
  let (symbols, combo) = rule in
  (List.fold_left (fun acc s -> acc ^ (if acc = "" then "" else " + ") ^ s) "" symbols) ^ " => " ^ combo

let parse (tokens : Grammar_lexer.token list) : (key_mapping list * production_rule list) option = 
  let rec split_file_sections list acc =
    match list with
    | [] -> None
    | Grammar_lexer.Newline :: Grammar_lexer.Newline :: t ->
      let rec filtered_tail tail = match tail with
      | [] -> []
      | Grammar_lexer.Newline :: t -> filtered_tail t
      | _ -> tail
      in Some (acc @ [Grammar_lexer.Newline], (filtered_tail t))
    | h :: t -> split_file_sections t (acc @ [h])
  in

  let rec split_by_token (list : Grammar_lexer.token list) (sep : Grammar_lexer.token) (acc : Grammar_lexer.token list) =
    match list with
    | [] -> (acc, [])
    | h :: t when h = sep -> (acc, t)
    | h :: t -> split_by_token t sep (acc @ [h])
  in

  let string_to_symbol str =
    if str = "QUIT" then Quit else if str = "RESET" then Reset else Symbol str
  in

  let rec parse_key_mappings key_mappings_tokens acc =
    match key_mappings_tokens with
    | [] -> Some acc
    | h1 :: h2 :: h3 :: h4 :: t -> begin
      match h1, h2, h3, h4 with
      | Grammar_lexer.Word key, Grammar_lexer.Colon, Grammar_lexer.Word value, Grammar_lexer.Newline ->
        if String.length key <> 1 then None else
        parse_key_mappings t (acc @ [((String.get key 0), (string_to_symbol value))])
      | _ -> None
      end
    | _ -> None
  in

  let rec parse_production_rules rules_tokens acc =
    let line, rest = split_by_token rules_tokens Grammar_lexer.Newline [] in
    match line with
    | [] -> Some acc
    | _ ->
      let symbols_tokens, combo_tokens = split_by_token line Grammar_lexer.Colon [] in
      let extract_word token = match token with | Grammar_lexer.Word w -> Some w | _ -> None in
      let fold_left_opt f acc list = List.fold_left (fun acc x -> match acc with | None -> None | Some acc -> f acc x) (Some acc) list in
      let symbols = fold_left_opt (fun acc x -> match (extract_word x) with
        | None -> None
        | Some s -> Some (acc @ [s])
        ) [] symbols_tokens in
      let combo = fold_left_opt (fun acc x -> match (extract_word x) with
        | None -> None
        | Some s -> Some (acc ^ (if acc = "" then "" else " ") ^ s)
        ) "" combo_tokens in
      match symbols, combo with
      | Some symbols, Some combo -> parse_production_rules rest (acc @ [(symbols, combo)])
      | _ -> None
  in

  match split_file_sections tokens [] with
  | None -> None
  | Some (key_mappings_tokens, rules_tokens) ->
    begin match parse_key_mappings key_mappings_tokens [] with
    | None -> None
    | Some key_mappings -> begin
      match parse_production_rules rules_tokens [] with
      | None -> None
      | Some production_rules -> Some (key_mappings, production_rules)
      end
    end
