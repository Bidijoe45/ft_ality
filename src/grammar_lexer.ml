type token = Word of string | Colon | Newline

let token_to_string (token : token) : string =
  match token with
  | Word s -> s
  | Colon -> ":"
  | Newline -> "[newline]"

let tokenize (in_chan : in_channel) : token list =
  let input_line_opt ic = try Some (input_line ic) with End_of_file -> None in

  let string_to_token s : token =
    if s = ":" then Colon
    else Word s
  in

  let rec parse_lines acc =
    match (input_line_opt in_chan) with
    | None -> acc
    | Some line ->
      let words = List.filter (fun s -> s <> "") (String.split_on_char ' ' line) in
      let tokens = List.map string_to_token words in
      parse_lines (acc @ tokens @ [Newline])
  in parse_lines []
