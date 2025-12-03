open Ast

val modus_ponens : prop -> prop -> prop option
(** [modus_ponens p1 p2] applies the Modus Ponens inference rule.

    - If [p1] is a proposition [A] and [p2] is an implication [A -> B], it
      returns [Some B].

    - If the arguments are reversed (i.e., [p1] is [A -> B] and [p2] is [A]), it
      also returns [Some B].

    - Otherwise, the result is [None]. *)
