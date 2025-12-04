open Ast

val modus_ponens : prop -> prop -> prop option
(** [modus_ponens p1 p2] applies the Modus Ponens inference rule.

    Preconditions:
    - [p1] and [p2] are well-formed propositional formulas.

    Postconditions:
    - If [p1] is a proposition [A] and [p2] is an implication [A -> B], it
      returns [Some B].

    - If the arguments are reversed (i.e., [p1] is [A -> B] and [p2] is [A]), it
      also returns [Some B].

    - Otherwise, the result is [None]. *)

val conjunction_introduction : prop -> prop -> prop option
(** [conjunction_introduction p1 p2] applies the Conjunction Introduction
    inference rule.

    Preconditions:
    - [p1] and [p2] are well-formed propositional formulas.

    Postconditions:
    - Returns [Some (And (p1, p2))], deriving [A & B] from [A] and [B]. *)

val modus_tollens : prop -> prop -> prop option
(** [modus_tollens p1 p2] applies Modus Tollens: from ¬B and (A -> B), derive
    ¬A. *)

val conjunction_elimination_left : prop -> prop option
(** [conjunction_elimination_left p] derives the left component from A & B. *)

val conjunction_elimination_right : prop -> prop option
(** [conjunction_elimination_right p] derives the right component from A & B. *)

val disjunction_introduction_left : prop -> prop -> prop option
(** [disjunction_introduction_left a b] derives A | B from A. *)

val disjunction_introduction_right : prop -> prop -> prop option
(** [disjunction_introduction_right a b] derives A | B from B. *)

val disjunction_elimination : prop -> prop -> prop -> prop option
(** [disjunction_elimination disj imp1 imp2] derives C from A | B, A -> C, and B -> C. *)

val hypothetical_syllogism : prop -> prop -> prop option
(** [hypothetical_syllogism p1 p2] derives A -> C from A -> B and B -> C. *)

val contraposition : prop -> prop option
(** [contraposition p] derives !B -> !A from A -> B. *)

val double_negation_introduction : prop -> prop option
(** [double_negation_introduction p] derives !!A from A. *)

val double_negation_elimination : prop -> prop option
(** [double_negation_elimination p] derives A from !!A. *)

val biconditional_introduction : prop -> prop -> prop option
(** [biconditional_introduction p1 p2] derives (A -> B) & (B -> A) from A -> B and B -> A. *)

val biconditional_elimination_left : prop -> prop option
(** [biconditional_elimination_left p] derives A -> B from (A -> B) & (B -> A). *)

val biconditional_elimination_right : prop -> prop option
(** [biconditional_elimination_right p] derives B -> A from (A -> B) & (B -> A). *)

val negation_introduction : prop -> prop -> prop option
(** [negation_introduction p1 p2] derives !A from A -> B and A -> !B. *)
