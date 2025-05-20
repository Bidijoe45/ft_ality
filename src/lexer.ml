type tokenType = STRING |
                COLON |
                SCOLON |
                LEFT_BRAKET |
                RIGHT_BRAKET |
                DASH

type token = {
  t: tokenType;
  value: string;
}

let string_of_tokenType = function
  | STRING -> "STRING"
  | COLON -> "COLON"
  | SCOLON -> "SCOLON"
  | LEFT_BRAKET -> "LEFT_BRAKET"
  | RIGHT_BRAKET -> "RIGHT_BRAKET"
  | DASH -> "DASH"

let string_of_token (tok : token) =
  Printf.sprintf "{ t = %s; value = \"%s\" }" (string_of_tokenType tok.t) tok.value

let print_token_list tokens =
  print_endline "[";
  List.iter (fun tok -> print_endline ("  " ^ string_of_token tok ^ ";")) tokens;
  print_endline "]"

let rec skip_whitespace s =
  if s = "" then ""
  else
    match s.[0] with
    | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (String.sub s 1 (String.length s - 1))
    | _ -> s

let get_next_token (input : string) : (token * string) option =
  let input = skip_whitespace input in
  let len = String.length input in
  if len = 0 then None
  else
    let consume n = String.sub input n (len - n) in

    if input.[0] = ':' then
      Some ({ t = COLON; value = ":" }, consume 1)
    else if input.[0] = ';' then
      Some ({ t = SCOLON; value = ";" }, consume 1)
    else if input.[0] = '[' then
      Some ({ t = LEFT_BRAKET; value = "[" }, consume 1)
    else if input.[0] = ']' then
      Some ({ t = RIGHT_BRAKET; value = "]" }, consume 1)
    else if input.[0] = '-' then
      Some ({ t = DASH; value = "-" }, consume 1)
    else
      let rec consume_string i =
        if i < len then
          match input.[i] with
          | '\t' | '\n' | '\r' | ':' | ';' | '[' | ']' | '-' -> i
          | _ -> consume_string (i + 1)
        else i
      in
      let end_idx = consume_string 0 in
      if end_idx = 0 then None
      else
        let str_token = String.sub input 0 end_idx in
        let rest = String.sub input end_idx (len - end_idx) in
        Some ({ t = STRING; value = str_token }, rest)
