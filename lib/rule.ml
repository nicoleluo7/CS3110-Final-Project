open Ast

(** modus_ponens takes in two prop and applies modus ponens rule to it. *)
let modus_ponens p1 p2 =
  match p2 with
  | Imp (m, n) when m = p1 -> Some n
  | _ -> (
      match p1 with
      | Imp (m, n) when m = p2 -> Some n
      | _ -> None)
