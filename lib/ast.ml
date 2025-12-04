(* Abstract syntax tree (AST) for propositional logic formulas. This is the core
   data type that represents formulas internally. *)
type prop =
  | Var of string (* A single propositional variable, e.g. "A" *)
  | And of prop * prop (* Conjunction: p & q *)
  | Or of prop * prop (* Disjunction: p | q *)
  | Imp of prop * prop (* Implication: p -> q *)
  | Not of prop (* Negation: !p *)

(* Convert a prop back into a readable string. Used for printing formulas in the
   REPL and later in the proof interface. *)
let rec prop_to_string = function
  | Var p -> p
  | And (p1, p2) -> "(" ^ prop_to_string p1 ^ " & " ^ prop_to_string p2 ^ ")"
  | Or (p1, p2) -> "(" ^ prop_to_string p1 ^ " | " ^ prop_to_string p2 ^ ")"
  | Imp (p1, p2) -> "(" ^ prop_to_string p1 ^ " -> " ^ prop_to_string p2 ^ ")"
  | Not p -> "!" ^ prop_to_string p

(* [simplify s] handles recursively handles negation for s *)
let rec simplify s = match s with
  | Not (Not p) ->
      (* double-negation elimination *)
      simplify p
  | Not p ->
      Not (simplify p)
  | And (p1, p2) ->
      And (simplify p1, simplify p2)
  | Or (p1, p2) ->
      Or (simplify p1, simplify p2)
  | Imp (p1, p2) ->
      Imp (simplify p1, simplify p2)
  | Var x -> Var x

(** [get_variables p] returns a list of all unique variables in the formula [p]. *)
let rec get_variables p =
  match p with
  | Var x -> [ x ]
  | Not p1 -> get_variables p1
  | And (p1, p2) | Or (p1, p2) | Imp (p1, p2) ->
      let vars1 = get_variables p1 in
      let vars2 = get_variables p2 in
      let all_vars = vars1 @ vars2 in
      let rec remove_duplicates = function
        | [] -> []
        | x :: xs ->
            if List.mem x xs then remove_duplicates xs else x :: remove_duplicates xs
      in
      remove_duplicates all_vars

(** [depth p] returns the maximum nesting depth of the formula [p]. *)
let rec depth p =
  match p with
  | Var _ -> 0
  | Not p1 -> 1 + depth p1
  | And (p1, p2) | Or (p1, p2) | Imp (p1, p2) ->
      1 + max (depth p1) (depth p2)

(** [size p] returns the total number of nodes in the formula tree. *)
let rec size p =
  match p with
  | Var _ -> 1
  | Not p1 -> 1 + size p1
  | And (p1, p2) | Or (p1, p2) | Imp (p1, p2) -> 1 + size p1 + size p2

(** [is_atomic p] returns true if [p] is a single variable. *)
let is_atomic p =
  match p with
  | Var _ -> true
  | _ -> false

(** [is_negation p] returns true if [p] is a negation. *)
let is_negation p =
  match p with
  | Not _ -> true
  | _ -> false

(** [is_conjunction p] returns true if [p] is a conjunction. *)
let is_conjunction p =
  match p with
  | And _ -> true
  | _ -> false

(** [is_disjunction p] returns true if [p] is a disjunction. *)
let is_disjunction p =
  match p with
  | Or _ -> true
  | _ -> false

(** [is_implication p] returns true if [p] is an implication. *)
let is_implication p =
  match p with
  | Imp _ -> true
  | _ -> false

(** [count_operators p] returns a tuple (and_count, or_count, imp_count, not_count)
    counting each type of operator in the formula. *)
let rec count_operators p =
  match p with
  | Var _ -> (0, 0, 0, 0)
  | Not p1 ->
      let a, o, i, n = count_operators p1 in
      (a, o, i, n + 1)
  | And (p1, p2) ->
      let a1, o1, i1, n1 = count_operators p1 in
      let a2, o2, i2, n2 = count_operators p2 in
      (a1 + a2 + 1, o1 + o2, i1 + i2, n1 + n2)
  | Or (p1, p2) ->
      let a1, o1, i1, n1 = count_operators p1 in
      let a2, o2, i2, n2 = count_operators p2 in
      (a1 + a2, o1 + o2 + 1, i1 + i2, n1 + n2)
  | Imp (p1, p2) ->
      let a1, o1, i1, n1 = count_operators p1 in
      let a2, o2, i2, n2 = count_operators p2 in
      (a1 + a2, o1 + o2, i1 + i2 + 1, n1 + n2)

(** [subformulas p] returns a list of all subformulas of [p], including [p] itself. *)
let rec subformulas p =
  match p with
  | Var _ -> [ p ]
  | Not p1 -> p :: subformulas p1
  | And (p1, p2) | Or (p1, p2) | Imp (p1, p2) ->
      p :: (subformulas p1 @ subformulas p2)

(** [contains p q] returns true if formula [q] appears as a subformula of [p]. *)
let contains p q =
  List.exists (( = ) q) (subformulas p)

(** [replace_subformula p old new] replaces all occurrences of [old] with [new] in [p]. *)
let rec replace_subformula p old_formula new_formula =
  if p = old_formula then new_formula
  else
    match p with
    | Var _ -> p
    | Not p1 -> Not (replace_subformula p1 old_formula new_formula)
    | And (p1, p2) ->
        And
          ( replace_subformula p1 old_formula new_formula,
            replace_subformula p2 old_formula new_formula )
    | Or (p1, p2) ->
        Or
          ( replace_subformula p1 old_formula new_formula,
            replace_subformula p2 old_formula new_formula )
    | Imp (p1, p2) ->
        Imp
          ( replace_subformula p1 old_formula new_formula,
            replace_subformula p2 old_formula new_formula )

(** [is_well_formed p] checks if the formula is well-formed (non-empty, etc.).
    Currently all constructors produce well-formed formulas, but this can be
    extended for more complex checks. *)
let is_well_formed p =
  match p with
  | Var s -> String.length s > 0
  | _ -> true

(** [get_left_operand p] returns the left operand if [p] is a binary operator. *)
let get_left_operand p =
  match p with
  | And (l, _) | Or (l, _) | Imp (l, _) -> Some l
  | _ -> None

(** [get_right_operand p] returns the right operand if [p] is a binary operator. *)
let get_right_operand p =
  match p with
  | And (_, r) | Or (_, r) | Imp (_, r) -> Some r
  | _ -> None

(** [get_negated_formula p] returns the formula inside a negation if [p] is a
    negation, otherwise None. *)
let get_negated_formula p =
  match p with
  | Not q -> Some q
  | _ -> None

(** [get_antecedent p] returns the antecedent if [p] is an implication. *)
let get_antecedent p =
  match p with
  | Imp (a, _) -> Some a
  | _ -> None

(** [get_consequent p] returns the consequent if [p] is an implication. *)
let get_consequent p =
  match p with
  | Imp (_, c) -> Some c
  | _ -> None