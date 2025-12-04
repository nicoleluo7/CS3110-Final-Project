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
