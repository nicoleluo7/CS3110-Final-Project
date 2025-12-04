open Ast

(** modus_ponens takes in two prop and applies modus ponens rule to it. *)
let modus_ponens p1 p2 =
  match p2 with
  | Imp (m, n) when m = p1 -> Some n
  | _ -> (
      match p1 with
      | Imp (m, n) when m = p2 -> Some n
      | _ -> None)

(** modus_tollens takes in two props and applies Modus Tollens: from ¬B and (A
    -> B), derive ¬A. *)
let modus_tollens p1 p2 =
  match (p1, p2) with
  | Not b1, Imp (a, b2) when b1 = b2 -> Some (Not a)
  | Imp (a, b2), Not b1 when b1 = b2 -> Some (Not a)
  | _ -> None

(** conjunction_introduction takes in two props A and B and applies conjunction
    introduction rule to derive A & B. *)
let conjunction_introduction p1 p2 = Some (And (p1, p2))

(** conjunction_elimination_left takes in a conjunction A & B and derives A. *)
let conjunction_elimination_left p =
  match p with
  | And (a, _) -> Some a
  | _ -> None

(** conjunction_elimination_right takes in a conjunction A & B and derives B. *)
let conjunction_elimination_right p =
  match p with
  | And (_, b) -> Some b
  | _ -> None

(** disjunction_introduction_left takes in a prop A and derives A | B for any B.
    In practice, we need to know what B should be, so this takes both. *)
let disjunction_introduction_left a b = Some (Or (a, b))

(** disjunction_introduction_right takes in a prop B and derives A | B for any
    A. In practice, we need to know what A should be, so this takes both. *)
let disjunction_introduction_right a b = Some (Or (a, b))

(** disjunction_elimination takes three props: A | B, A -> C, and B -> C, and
    derives C. This is a three-argument rule, so we'll handle it differently. *)
let disjunction_elimination disj imp1 imp2 =
  match (disj, imp1, imp2) with
  | Or (a, b), Imp (a1, c1), Imp (b1, c2) when a = a1 && b = b1 && c1 = c2 ->
      Some c1
  | Or (a, b), Imp (b1, c1), Imp (a1, c2) when a = a1 && b = b1 && c1 = c2 ->
      Some c1
  | _ -> None

(** hypothetical_syllogism takes two implications A -> B and B -> C and derives
    A -> C. *)
let hypothetical_syllogism p1 p2 =
  match (p1, p2) with
  | Imp (a, b1), Imp (b2, c) when b1 = b2 -> Some (Imp (a, c))
  | Imp (b2, c), Imp (a, b1) when b1 = b2 -> Some (Imp (a, c))
  | _ -> None

(** contraposition takes an implication A -> B and derives !B -> !A. *)
let contraposition p =
  match p with
  | Imp (a, b) -> Some (Imp (Not b, Not a))
  | _ -> None

(** biconditional_introduction takes two implications A -> B and B -> A and
    derives A <-> B. Since we don't have a biconditional type, we'll represent
    it as (A -> B) & (B -> A). *)
let biconditional_introduction p1 p2 =
  match (p1, p2) with
  | Imp (a1, b1), Imp (b2, a2) when a1 = a2 && b1 = b2 ->
      Some (And (Imp (a1, b1), Imp (b2, a2)))
  | Imp (b2, a2), Imp (a1, b1) when a1 = a2 && b1 = b2 ->
      Some (And (Imp (a1, b1), Imp (b2, a2)))
  | _ -> None

(** biconditional_elimination_left takes a biconditional (A -> B) & (B -> A) and
    derives A -> B. *)
let biconditional_elimination_left p =
  match p with
  | And (Imp (a, b), Imp (b2, a2)) when a = a2 && b = b2 -> Some (Imp (a, b))
  | _ -> None

(** biconditional_elimination_right takes a biconditional (A -> B) & (B -> A)
    and derives B -> A. *)
let biconditional_elimination_right p =
  match p with
  | And (Imp (a, b), Imp (b2, a2)) when a = a2 && b = b2 -> Some (Imp (b2, a2))
  | _ -> None

(** negation_introduction attempts to derive !A from A -> B and A -> !B. This is
    a proof by contradiction pattern. *)
let negation_introduction p1 p2 =
  match (p1, p2) with
  | Imp (a1, b1), Imp (a2, Not b2) when a1 = a2 && b1 = b2 -> Some (Not a1)
  | Imp (a2, Not b2), Imp (a1, b1) when a1 = a2 && b1 = b2 -> Some (Not a1)
  | _ -> None

(** double_negation_introduction takes A and derives !!A. *)
let double_negation_introduction p = Some (Not (Not p))

(** double_negation_elimination takes !!A and derives A. *)
let double_negation_elimination p =
  match p with
  | Not (Not a) -> Some a
  | _ -> None
