type state = int

type 'a t = state -> 'a * state

let bind (t : 'a t) (f : 'a -> 'b t) : 'b t =
  fun state ->
    (* apply the first state transition first *)
    let a, transient_state = t state in
    (* and then the second *)
    let b, final_state = f a transient_state in
    (* return these *)
    (b, final_state)

let return (a : 'a) = fun (state : state) -> (a, state)

let (let*) = bind

type key = Grammar.key
type symbol = string
type combo_name = Grammar.combo_name

type transition = state * (symbol * state)

type trie = {
  transitions : transition list;
  accepting_states : (state * combo_name list) list
}

let transition_to_string (transition : transition) : string =
  let s0, (sym, s1) = transition in
  "(" ^ (string_of_int s0) ^ ", " ^ sym ^ ") => " ^ (string_of_int s1)

let trie_to_string (trie : trie) : string =
  let list_to_string l = List.fold_left (fun acc s -> acc ^ (if acc = "" then "" else ", ") ^ s) "" l in
  "Transitions:\n"
  ^ (List.fold_left (fun acc t -> acc ^ "  " ^ (transition_to_string t) ^ "\n") "" trie.transitions)
  ^ "Accepting_states:\n"
  ^ (List.fold_left (fun acc (s, names) -> acc ^ "  " ^ (string_of_int s) ^ " = [" ^ (list_to_string names) ^ "]\n") "" trie.accepting_states)

let find_key_mapping (key : key) (mappings : Grammar.key_mapping list) = List.find_opt (fun (k, _) -> k = key) mappings

let find_transition (state : state) (symbol : symbol) (transitions : transition list) = List.find_opt (fun (s0, (sym, _)) -> s0 = state && sym = symbol) transitions

let find_accepting_state (state : state) (accepting_states : (state * combo_name list) list) = List.find_opt (fun (s, _) -> s = state) accepting_states

let train (rules : Grammar.production_rule list) : trie =
  let rec process_rule (rule : Grammar.production_rule) (state : state) (next_state : state) (trie : trie) = 
    match rule with
    | ([], move_name) -> begin
        match find_accepting_state state trie.accepting_states with
          | None -> ({trie with accepting_states = trie.accepting_states @ [(state, [move_name])]}, next_state)
          | Some (s, names) -> ({trie with accepting_states = List.map (fun (s, names) -> if s = state then (s, move_name :: names) else (s, names)) trie.accepting_states}, next_state)
      end
    | (h :: t, move_name) ->
      match find_transition state h trie.transitions with
      | None ->
        let new_state = next_state + 1 in
        process_rule (t, move_name) new_state new_state {trie with transitions = trie.transitions @ [(state, (h, new_state))]}
      | Some (s0, (tok, s1)) -> process_rule (t, move_name) s1 next_state trie
  in
  let rec process_all_rules rules (next_state : state) (trie : trie) = match rules with
    | [] -> trie
    | h :: t ->
      let (updated_trie, updated_next_state) = process_rule h 0 next_state trie in
      process_all_rules t updated_next_state updated_trie
  in process_all_rules rules 0 {transitions = []; accepting_states = []}

let run (key_mappings : Grammar.key_mapping list) (trie : trie) =

  let trans (symbol : symbol) (state : state) =
    match find_transition state symbol trie.transitions with
      | None -> (None, 0)
      | Some (s0, (sym, s1)) -> (Some sym, s1)
  in
  let accept (state : state) =
    match find_accepting_state state trie.accepting_states with
      | None -> (None, state)
      | Some (_, combos) -> (Some combos, state)
  in
  let process_symbol (symbol : symbol) =
    let* transition = trans symbol in
    let* combos = accept in
    match transition with
      | None -> return None
      | Some _ ->
        match combos with
          | None -> return (Some [])
          | Some combo_list -> return (Some combo_list)
  in
  let get_one_char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio;
    res
  in

  let rec process_input (state : state) =
    let input = Unix.handle_unix_error get_one_char () in
    print_endline "";
    match find_key_mapping input key_mappings with
    | None -> print_endline "Unrecognised key";
      process_input 0
    | Some (_, symbol) -> begin match symbol with
      | Grammar.Quit -> ()
      | Grammar.Reset -> process_input 0
      | Symbol sym -> 
        let result, new_state = process_symbol sym state in
        match result with
        | None -> print_endline "Unrecognised combo";
          process_input 0
        | Some combos -> List.iter (fun x -> print_endline (x ^ "!")) combos;
          process_input new_state
      end
  in process_input 0
