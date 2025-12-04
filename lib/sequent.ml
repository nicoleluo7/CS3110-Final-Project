open Ast
open Rule

let inference_rules = [ modus_ponens; modus_tollens ]

type t = {
  premises : prop list;
  derived : prop list;
  goal : prop option;
}
(** type t represents a record of premises, what's derived and the goal.
    premises is a list of prop, derived is a list of prop and goal is an option
    that is none by default and can be set to some p. *)

(** empty initiates an empty t with premises and derived empty and goal set to
    none. *)
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
