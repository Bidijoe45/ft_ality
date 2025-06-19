type token = string
type production_rule = (token list * string)

let rule_to_string (rule : production_rule) : string =
  let (tokens, move) = rule in
  (List.fold_left (fun acc t -> acc ^ (if acc = "" then "" else " + ") ^ t) "" tokens) ^ " => " ^ move

let tokenize_rule (line : string) : production_rule option =
  let split_line = String.split_on_char ':' line in
  match split_line with
  | [] -> None
  | tokens :: move :: [] -> Some ((String.split_on_char ' ' (String.trim tokens)), (String.trim move))
  | _ -> None
