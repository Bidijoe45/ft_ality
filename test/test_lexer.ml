
let get_lexer_output (input : string) : Lexer.token list =
  let rec run_lexer (s : string) (acc : Lexer.token list) : Lexer.token list =
    match Lexer.get_next_token s with
    | None -> List.rev acc
    | Some (token, rest) -> run_lexer rest (token :: acc)
  in
  run_lexer input []

let lexer_test_1 =
  print_endline "Lexer test 1";
  let lexer_input = "q:[LP]:Left-Punch;" in

  let lexer_expected_output : Lexer.token list = [
    { t = Lexer.STRING; value = "q" };
    { t = Lexer.COLON; value = ":" };
    { t = Lexer.LEFT_BRAKET; value = "[" };
    { t = Lexer.STRING; value = "LP" };
    { t = Lexer.RIGHT_BRAKET; value = "]" };
    { t = Lexer.COLON; value = ":" };
    { t = Lexer.STRING; value = "Left" };
    { t = Lexer.DASH; value = "-" };
    { t = Lexer.STRING; value = "Punch" };
    { t = Lexer.SCOLON; value = ";" };
  ] in

  let lexer_output = get_lexer_output lexer_input in

  if lexer_output = lexer_expected_output then
    print_endline "Test passed"
  else begin
    print_endline "Test failed";
    print_endline "Expected tokens:";
    Lexer.print_token_list lexer_expected_output;
    print_endline "Actual tokens:";
    Lexer.print_token_list lexer_output;
  end;

  lexer_output = lexer_expected_output

let lexer_test_2 =
  print_endline "Lexer test 2";
  let lexer_input = "---" in

  let lexer_expected_output : Lexer.token list = [
    { t = Lexer.DASH; value = "-" };
    { t = Lexer.DASH; value = "-" };
    { t = Lexer.DASH; value = "-" };
  ] in

  let lexer_output = get_lexer_output lexer_input in

  if lexer_output = lexer_expected_output then
    print_endline "Test passed"
  else begin
    print_endline "Test failed";
    print_endline "Expected tokens:";
    Lexer.print_token_list lexer_expected_output;
    print_endline "Actual tokens:";
    Lexer.print_token_list lexer_output;
  end;

  lexer_output = lexer_expected_output

let lexer_test_3 =
  print_endline "Lexer test 3";
  let lexer_input = "Mega Damage:[LP]:[RP]:[LK]:[RK];" in

  let lexer_expected_output : Lexer.token list = [
    { t = Lexer.STRING; value = "Mega Damage" };
    { t = Lexer.COLON; value = ":" };
    { t = Lexer.LEFT_BRAKET; value = "[" };
    { t = Lexer.STRING; value = "LP" };
    { t = Lexer.RIGHT_BRAKET; value = "]" };
    { t = Lexer.COLON; value = ":" };
    { t = Lexer.LEFT_BRAKET; value = "[" };
    { t = Lexer.STRING; value = "RP" };
    { t = Lexer.RIGHT_BRAKET; value = "]" };
    { t = Lexer.COLON; value = ":" };
    { t = Lexer.LEFT_BRAKET; value = "[" };
    { t = Lexer.STRING; value = "LK" };
    { t = Lexer.RIGHT_BRAKET; value = "]" };
    { t = Lexer.COLON; value = ":" };
    { t = Lexer.LEFT_BRAKET; value = "[" };
    { t = Lexer.STRING; value = "RK" };
    { t = Lexer.RIGHT_BRAKET; value = "]" };
    { t = Lexer.SCOLON; value = ";" };
  ] in

  let lexer_output = get_lexer_output lexer_input in

  if lexer_output = lexer_expected_output then
    print_endline "Test passed"
  else begin
    print_endline "Test failed";
    print_endline "Expected tokens:";
    Lexer.print_token_list lexer_expected_output;
    print_endline "Actual tokens:";
    Lexer.print_token_list lexer_output;
  end;

  lexer_output = lexer_expected_output

let test = print_endline "Running lexer tests"; lexer_test_1 && lexer_test_2 && lexer_test_3