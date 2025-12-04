open Ast

type t = {
  premises : prop list;
  derived : prop list;
  goal : prop option;
}
(** A proof state.
    - [premises] are the user-supplied assumptions.
    - [derived] are formulas inferred from premises and other derived facts.
    - [goal] is an optional target formula we are trying to prove. *)

val empty : t
(** The empty proof state: no premises, no derived formulas, and no goal.

    Preconditions: None.

    Postconditions: it returns a record type value with [premises] = [],
    [derived] = [], [goal] = None. *)

val add_premise : t -> prop -> t
(** [add_premise st p] returns a new proof state like [st] but with [p] added to
    the list of premises. also prints whether addition was successful or
    rejected.

    Preconditions:
    - [st] is a valid proof state.
    - [p] is a well-formed propositional formula built from constructors Var,
      And, Imp, and Not. It may conflict with existing premises or derived
      formulas, which will be handled by the function.

    Postconditions:
    - If [p] conflicts with some q in st.premises or st.derived, the function
      will print a rejection message and the original state [st] is returned
      unchanged.
    - Otherwise, p will be added to st.premises. *)

val add_derived : t -> prop -> t
(** [add_derived st p] returns a new proof state like [st] but with [p] added to
    the list of derived formulas, unless [p] is already known as a premise or a
    derived formula.

    Preconditions:
    - [st] is a valid proof state.
    - [p] is a well-formed propositional formula built from constructors Var,
      And, Imp, and Not.

    Postconditions:
    - If [p] already appears in st.derived or st.premises, it returns [st]
      unchanged.
    - Otherwise, it returns new state with [derived = p :: st.derived]. *)

val add_goal : t -> prop -> t
(** [add_goal st p] returns a new proof state like [st] but with the goal set to
    [Some p].

    Preconditions:
    - [p] is a well-formed propositional formula built from constructors Var,
      And, Imp, and Not.

    Postconditions:
    - It returns state with [goal = Some p]. *)

val apply_modus_ponens : t -> t
(** [apply_modus_ponens st] repeatedly applies Modus Ponens to the formulas in
    [st.premises] and [st.derived], adding any newly inferred formulas to
    [derived], until no more can be added.

    Preconditions:
    - [st] is a valid proof state.
    - Premises and derived must be finite.

    Postconditions:
    - It returns a state [st'] with every formula in [st'.derived] is either:
      originally in st.derived or the consequent of a valid Modus Ponens step.
*)

val judge_goal : t -> bool
(** [judge_goal st] is [true] iff the goal of [st] is present among its premises
    or derived formulas; otherwise [false].

    Preconditions:
    - [st] is a valid proof state.

    Postconditions:
    - It returns [false] if [st.goal = None].
    - It returns [true] iff the goal proposition appears in st.premises or
      st.derived. *)

val print_result : t -> unit
(** [print_result st] prints a human-readable summary of the proof state,
    including premises, derived formulas, the goal, and whether the goal has
    been reached.

    Preconditions:
    - [st] is a valid proof state.

    Postconditions:
    - It writes output to stdout and returns unit without modifying [st]. *)

val explain_derivation : t -> prop -> (prop * prop) option
(** [explain_derivation st new_prop] attempts to find an explanation for how
    [new_prop] was derived using Modus Ponens from propositions inside [st].

    Preconditions:
    - [st] is a valid proof state.
    - [new_prop] is a well-formed proposition formula built from constructors
      Var, And, Imp, and Not.

    Postconditions:
    - If there exist A and A -> B in st.premises or st.derived such that B =
      new_prop, it returns [Some (A, A -> B)].
    - If no such pair exists, it returns [None]. *)
