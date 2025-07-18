type state = { next : int }

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

type transition = state * (string * state)
type trie = {
  transitions : transition list;
  accepting_states : (state * string list) list
}

(* let find_transition state token transitions = List.find_opt (fun (s0, (tok, _)) -> s0 = state && tok = token) transitions
let find_accepting_state state accepting_states = List.find_opt (fun (s, _) -> s = state) accepting_states *)

let trans (trie : trie) (symbol : string) (state : state) =
    match (List.find_opt (fun (s0, (tok, _)) -> s0 = state && tok = symbol) trie.transitions) with
      | None -> (None, state)
      | Some (s0, (token, s1)) ->
        begin
          match List.find_opt (fun (s, _) -> s = s1) trie.accepting_states with
          | None -> print_endline ("tok: " ^ token ^ " -> found trans but no final states"); (Some [], s1)
          | Some (_, combos) -> (Some combos, s1)
        end

let () =
  let run (trie : trie) (input : string) =
    let symbols = (String.split_on_char ' ' input) in

    let rec process_symbols list last_combos = 
      match list with
      | [] -> return last_combos
      | h :: t -> 
        let* combos = (trans trie h) in
        match combos with
          | None -> return combos
          | Some combo_list ->
              List.iter (fun x -> print_endline (x ^ "!")) combo_list;
              process_symbols t combos
    in process_symbols symbols None
  in
  let trie : trie = {
    transitions = [
      ({ next = 0 }, ("DOWN", { next = 1 }));
      ({ next = 0 }, ("UP", { next = 4 }));
      ({ next = 1 }, ("RIGHT", { next = 2 }));
      ({ next = 2 }, ("PUNCH", { next = 3 }))
    ];
    accepting_states = [
          ({ next = 3 }, ["wow!"]);
          ({ next = 4 }, ["wow x2!"])
    ]
  } in
  let res, state = run trie "DOWN RIGHT PUNCH" { next = 0 } in
  match res with
  | None -> print_endline ("Unrecognised combo. final state: " ^ (string_of_int state.next))
  | Some combo_list -> print_endline ("recognised ok. final state: " ^ (string_of_int state.next))

