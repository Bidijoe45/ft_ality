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

type transition = state * (Lexer.token * state)
type trie = {
  transitions : transition list;
  accepting_states : (state * string list) list
}

let transition_to_string (transition : transition) : string =
  let s0, (tok, s1) = transition in
  "(" ^ (string_of_int s0) ^ ", " ^ tok ^ ") => " ^ (string_of_int s1)

let trie_to_string (trie : trie) : string =
  let list_to_string l = List.fold_left (fun acc s -> acc ^ (if acc = "" then "" else ", ") ^ s) "" l in
  "Transitions:\n"
  ^ (List.fold_left (fun acc t -> acc ^ "  " ^ (transition_to_string t) ^ "\n") "" trie.transitions)
  ^ "Accepting_states:\n"
  ^ (List.fold_left (fun acc (s, names) -> acc ^ "  " ^ (string_of_int s) ^ " = [" ^ (list_to_string names) ^ "]\n") "" trie.accepting_states)

let find_transition state token transitions = List.find_opt (fun (s0, (tok, _)) -> s0 = state && tok = token) transitions

let find_accepting_state state accepting_states = List.find_opt (fun (s, _) -> s = state) accepting_states

let train (rules : Lexer.production_rule list) : trie =
  let rec process_rule (rule : Lexer.production_rule) (state : state) (next_state : state) (trie : trie) = 
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

let run (trie : trie) (input : string) =

  let trans (symbol : string) (state : state) =
    match find_transition state symbol trie.transitions with
      | None -> (None, state)
      | Some (s0, (token, s1)) -> (Some token, s1)
  in
  let accept (state : state) =
    match find_accepting_state state trie.accepting_states with
      | None -> (None, state)
      | Some (_, combos) -> (Some combos, state)
  in

  let symbols = (String.split_on_char ' ' input) in
  let rec process_symbols list last_combos =
    match list with
    | [] -> return last_combos
    | h :: t ->
      let* transition = trans h in
      match transition with
        | None -> return None
        | Some token ->
          let* combos = accept in
          match combos with
            | None -> process_symbols t None
            | Some combo_list ->
                List.iter (fun x -> print_endline (x ^ "!")) combo_list;
                process_symbols t combos
  in process_symbols symbols None
