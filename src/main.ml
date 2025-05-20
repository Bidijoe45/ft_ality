(* Set terminal to raw mode to capture key presses immediately *)
let set_raw_mode () =
  let term = Unix.tcgetattr Unix.stdin in
  let raw = { term with Unix.c_icanon = false; Unix.c_echo = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW raw;
  term (* return original term *)

(* Restore terminal to original mode *)
let restore_mode original_term =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW original_term

(* Read a single character from stdin *)
let read_char () =
  let buf = Bytes.create 3 in
  let n = Unix.read Unix.stdin buf 0 3 in
  Bytes.sub_string buf 0 n

(* Functional recursive loop *)
let rec input_loop () =
  let key = read_char () in
  match key with
  | "\027[A" -> print_endline "up"
  | "\027[B" -> print_endline "down"
  | "\027[C" -> print_endline "right"
  | "\027[D" -> print_endline "left"
  | c -> Printf.printf "Key: %S\n%!" c;
  input_loop () (* tail-recursive call *)

let () =
  let original_term = set_raw_mode () in
  try
    input_loop ()
  with e ->
    restore_mode original_term;
    raise e