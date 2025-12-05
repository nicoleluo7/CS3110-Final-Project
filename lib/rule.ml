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

