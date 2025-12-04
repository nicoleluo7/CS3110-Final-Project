open Ast
open Rule

let inference_rules = [
  modus_ponens;
  modus_tollens;
  hypothetical_syllogism;
]

type t = {
  premises : prop list;
  derived : prop list;
  goal : prop option;
}

let empty = { premises = []; derived = []; goal = None }

(* conflicts is a helper function that determines if a premise p conflicts with
   another premise q *)
let conflicts p q =
  match (p, q) with
  | Not p1, q1 when p1 = q1 -> true
  | p1, Not q1 when p1 = q1 -> true
  | _ -> false

(** conjunction_elimination [p] is a helper function that performs conjunction
    elimination on [p]. If [p] is a formula premise constructed from And (A, B),
    it derives from A & B and return [A; B], otherwise it returns []. *)
let conjunction_elimination p =
  match p with
  | And (a, b) -> [ a; b ]
  | _ -> []

(** add_derived adds a prop to the original list. *)
let rec add_derived t p =
  if List.mem p t.derived || List.mem p t.premises then t
  else
    let t' = { t with derived = p :: t.derived } in
    (* Conjunction elimination: derive A and B from A & B. *)
    List.fold_left add_derived t' (conjunction_elimination p)

(** add_premise adds a prop unless it conflicts with an existing premise or
    derived prop. If it conflicts, it prints a message and rejects it. *)
let add_premise t p =
  let all_known = t.premises @ t.derived in
  match List.find_opt (conflicts p) all_known with
  | Some q ->
      print_endline
        ("Rejected premise: " ^ prop_to_string p ^ " conflicts with existing: "
       ^ prop_to_string q ^ ". Sequent not changed.");
      t
  | None ->
      print_endline ("Added premise: " ^ prop_to_string p);
      let t' = { t with premises = p :: t.premises } in
      (* Conjunction elimination: also add A and B *)
      List.fold_left add_derived t' (conjunction_elimination p)

(** add_goal takes in two arguments, first is a record of type t and second is a
    valid prop p, then it sets the goal of t to Some p. *)
let add_goal t p = { t with goal = Some p }

(** apply_modus_ponens applies modus ponens rule to all props in the premises
    and derived list of the record and adds any potential new props to the
    derived list. *)
let rec apply_modus_ponens st =
  let known = st.premises @ st.derived in

  (* Compute all new propositions that can be inferred in this round using all
     inference rules (Modus Ponens, Modus Tollens, etc.). *)
  let new_props =
    List.fold_left
      (fun acc p1 ->
        List.fold_left
          (fun acc2 p2 ->
            List.fold_left
              (fun acc3 rule ->
                match rule p1 p2 with
                | Some p
                  when (not (List.mem p st.premises))
                       && (not (List.mem p st.derived))
                       && not (List.mem p acc3) -> p :: acc3
                | _ -> acc3)
              acc2 inference_rules)
          acc known)
      [] known
  in
  match new_props with
  | [] -> st
  | _ ->
      let new_st = List.fold_left add_derived st new_props in
      apply_modus_ponens new_st

(** apply_conjunction_introduction applies conjunction introduction rule to all
    pairs of props in the premises and derived list and adds any potential new
    conjunctions to the derived list. From A and B, derives A & B.
    
    Note: To avoid excessive nesting, we only combine "simple" formulas
    (atomic variables, implications, negations) and not conjunctions themselves. *)
let apply_conjunction_introduction st =
  let known = st.premises @ st.derived in

  (* Helper: check if a formula is simple (not a conjunction or disjunction) *)
  let is_simple = function
    | Var _ | Imp _ | Not _ -> true
    | And _ | Or _ -> false
  in

  (* Compute all new propositions that can be inferred *)
  let new_props =
    List.fold_left
      (fun acc p1 ->
        List.fold_left
          (fun acc2 p2 ->
            (* Don't conjoin a proposition with itself *)
            if p1 = p2 then acc2
            (* Only combine simple formulas to avoid nested conjunctions *)
            else if not (is_simple p1 && is_simple p2) then acc2
            else
              match conjunction_introduction p1 p2 with
              | Some p
                when (not (List.mem p st.premises))
                     && (not (List.mem p st.derived))
                     && not (List.mem p acc) -> p :: acc2
              | _ -> acc2)
          acc known)
      [] known
  in
  (* Single pass: add all new props and return (no recursion) *)
  List.fold_left add_derived st new_props

(** judge_goal returns the result of the goal. If the goal is None or doesn't
    exist in premises and derived lists, then it will return false, otherwise,
    it will return true. *)
let judge_goal t =
  match t.goal with
  | None -> false
  | Some p ->
      List.exists (( = ) p) t.premises || List.exists (( = ) p) t.derived

(** print_result prints the contents of the record of type t in a concise way.
*)
let print_result t =
  print_endline "Proof State:";
  print_endline "Premises:";
  List.iter (fun p -> print_endline (" - " ^ prop_to_string p)) t.premises;
  print_endline "Derived:";
  List.iter (fun p -> print_endline (" - " ^ prop_to_string p)) t.derived;
  print_endline
    ("Goal: "
    ^
    match t.goal with
    | None -> "None"
    | Some p -> prop_to_string p);
  print_endline (if judge_goal t then "Goal reached!" else "Goal not reached")

let explain_derivation st new_prop =
  (* new_prop must have come from MP: A and A -> B *)
  let rec find = function
    | [] -> None
    | (a, imp) :: rest -> (
        match imp with
        | Imp (prem, concl) when concl = new_prop && prem = a -> Some (a, imp)
        | _ -> find rest)
  in

  (* Build list of candidate (A, A->B) pairs *)
  let candidates =
    List.concat
      (List.map
         (fun a ->
           List.filter_map
             (fun p ->
               match p with
               | Imp (prem, concl) when prem = a -> Some (a, p)
               | _ -> None)
             (st.premises @ st.derived))
         (st.premises @ st.derived))
  in
  find candidates

(** [apply_conjunction_elimination st] applies conjunction elimination to all
    conjunctions in premises and derived, deriving their components. *)
let apply_conjunction_elimination st =
  let known = st.premises @ st.derived in
  let new_props =
    List.fold_left
      (fun acc p ->
        match conjunction_elimination_left p with
        | Some left ->
            if not (List.mem left st.premises)
               && not (List.mem left st.derived)
               && not (List.mem left acc)
            then left :: acc
            else acc
        | None -> acc)
      [] known
  in
  let new_props2 =
    List.fold_left
      (fun acc p ->
        match conjunction_elimination_right p with
        | Some right ->
            if not (List.mem right st.premises)
               && not (List.mem right st.derived)
               && not (List.mem right acc)
               && not (List.mem right new_props)
            then right :: acc
            else acc
        | None -> acc)
      [] known
  in
  List.fold_left add_derived st (new_props @ new_props2)

(** [apply_disjunction_introduction st] applies disjunction introduction to all
    formulas, creating disjunctions with other formulas. *)
let apply_disjunction_introduction st =
  let known = st.premises @ st.derived in
  let new_props =
    List.fold_left
      (fun acc p1 ->
        List.fold_left
          (fun acc2 p2 ->
            if p1 = p2 then acc2
            else
              match disjunction_introduction_left p1 p2 with
              | Some p
                when (not (List.mem p st.premises))
                     && (not (List.mem p st.derived))
                     && not (List.mem p acc)
                     && not (List.mem p acc2) -> p :: acc2
              | _ -> acc2)
          acc known)
      [] known
  in
  List.fold_left add_derived st new_props

(** [apply_hypothetical_syllogism st] applies hypothetical syllogism to derive
    new implications from chains of implications. *)
let rec apply_hypothetical_syllogism st =
  let known = st.premises @ st.derived in
  let new_props =
    List.fold_left
      (fun acc p1 ->
        List.fold_left
          (fun acc2 p2 ->
            match hypothetical_syllogism p1 p2 with
            | Some p
              when (not (List.mem p st.premises))
                   && (not (List.mem p st.derived))
                   && not (List.mem p acc)
                   && not (List.mem p acc2) -> p :: acc2
            | _ -> acc2)
          acc known)
      [] known
  in
  match new_props with
  | [] -> st
  | _ ->
      let new_st = List.fold_left add_derived st new_props in
      apply_hypothetical_syllogism new_st

(** [apply_contraposition st] applies contraposition to all implications. *)
let apply_contraposition st =
  let known = st.premises @ st.derived in
  let new_props =
    List.fold_left
      (fun acc p ->
        match contraposition p with
        | Some p'
          when (not (List.mem p' st.premises))
               && (not (List.mem p' st.derived))
               && not (List.mem p' acc) -> p' :: acc
        | _ -> acc)
      [] known
  in
  List.fold_left add_derived st new_props

(** [get_all_formulas st] returns all formulas (premises and derived) in the state. *)
let get_all_formulas st = st.premises @ st.derived

(** [count_formulas st] returns the total number of formulas in the state. *)
let count_formulas st =
  List.length st.premises + List.length st.derived

(** [has_formula st p] returns true if [p] appears in premises or derived. *)
let has_formula st p =
  List.mem p st.premises || List.mem p st.derived

(** [remove_premise st p] removes [p] from premises if it exists. *)
let remove_premise st p =
  { st with premises = List.filter (( <> ) p) st.premises }

(** [clear_derived st] clears all derived formulas. *)
let clear_derived st = { st with derived = [] }

(** [clear_goal st] clears the goal. *)
let clear_goal st = { st with goal = None }

(** [get_premises st] returns the list of premises. *)
let get_premises st = st.premises

(** [get_derived st] returns the list of derived formulas. *)
let get_derived st = st.derived

(** [get_goal st] returns the goal if it exists. *)
let get_goal st = st.goal

(** [is_empty st] returns true if there are no premises, derived formulas, or goal. *)
let is_empty st =
  st.premises = [] && st.derived = [] && st.goal = None

(** [get_statistics st] returns a summary of the proof state. *)
let get_statistics st =
  let premise_count = List.length st.premises in
  let derived_count = List.length st.derived in
  let goal_set = match st.goal with Some _ -> true | None -> false in
  let goal_reached = judge_goal st in
  (premise_count, derived_count, goal_set, goal_reached)

(** [apply_all_rules st] applies all available inference rules exhaustively. *)
let rec apply_all_rules st =
  let st1 = apply_modus_ponens st in
  let st2 = apply_conjunction_introduction st1 in
  let st3 = apply_conjunction_elimination st2 in
  let st4 = apply_hypothetical_syllogism st3 in
  let st5 = apply_contraposition st4 in
  if count_formulas st5 = count_formulas st then st5
  else apply_all_rules st5

(** [find_derivations st p] attempts to find how [p] could be derived from the
    current state using various rules. Returns a list of possible explanations. *)
let find_derivations st p =
  let known = st.premises @ st.derived in
  let explanations = ref [] in
  (* Check Modus Ponens *)
  List.iter
    (fun a ->
      List.iter
        (fun imp ->
          match imp with
          | Imp (prem, concl) when prem = a && concl = p ->
              explanations := ("Modus Ponens", a, imp) :: !explanations
          | _ -> ())
        known)
    known;
  (* Check Modus Tollens *)
  List.iter
    (fun not_b ->
      List.iter
        (fun imp ->
          match (not_b, imp) with
          | Not b1, Imp (a, b2) when b1 = b2 && p = Not a ->
              explanations := ("Modus Tollens", not_b, imp) :: !explanations
          | _ -> ())
        known)
    known;
  (* Check Conjunction Introduction *)
  List.iter
    (fun p1 ->
      List.iter
        (fun p2 ->
          if p = And (p1, p2) || p = And (p2, p1) then
            explanations := ("Conjunction Introduction", p1, p2) :: !explanations)
        known)
    known;
  !explanations

(** [export_state st] returns a string representation of the state suitable for
    saving to a file. *)
let export_state st =
  let lines = ref [] in
  lines := "Premises:" :: !lines;
  List.iter (fun p -> lines := ("  " ^ prop_to_string p) :: !lines) st.premises;
  lines := "Derived:" :: !lines;
  List.iter (fun p -> lines := ("  " ^ prop_to_string p) :: !lines) st.derived;
  lines :=
    ( "Goal: "
    ^ match st.goal with None -> "None" | Some g -> prop_to_string g )
    :: !lines;
  lines :=
    ( "Goal Reached: " ^ if judge_goal st then "Yes" else "No" )
    :: !lines;
  String.concat "\n" (List.rev !lines)
