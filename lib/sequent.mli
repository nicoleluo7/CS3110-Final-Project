open Ast

type t = {
  premises : prop list;
  derived : prop list;
  goal : prop option;
}
(** A proof state.
    - [premises] are the user-supplied assumptions.
    - [derived] are formulas inferred from premises and other derived facts.
    - [goal] is an optional target formula we are trying to prove.
    
    Representation Invariant:
    - All formulas in [premises] and [derived] are well-formed propositional formulas.
    - No formula appears in both [premises] and [derived] simultaneously.
    - [derived] contains only formulas that can be logically derived from [premises]
      using the available inference rules.
    
    Abstraction Function:
    - AF(t) = (Γ, Δ, G) where:
      - Γ is the set of premises (from [premises])
      - Δ is the set of derived formulas (from [derived])
      - G is the optional goal (from [goal]) *)

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

val apply_conjunction_introduction : t -> t
(** [apply_conjunction_introduction st] repeatedly applies Conjunction
    Introduction to all pairs of formulas in [st.premises] and [st.derived],
    adding any newly inferred conjunctions to [derived], until no more can be
    added. From A and B, derives A & B.

    Preconditions:
    - [st] is a valid proof state.
    - Premises and derived must be finite.

    Postconditions:
    - It returns a state [st'] where [st'.derived] contains all possible
      conjunctions that can be formed from pairs of distinct formulas in
      st.premises and st.derived.
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

val explain_derivation_enhanced : t -> prop -> (string * prop * prop option) option
(** [explain_derivation_enhanced st p] returns an explanation for how [p] was derived,
    checking multiple inference rules. Returns [Some (rule_name, prop1, prop2_opt)] or [None]. *)

val apply_conjunction_elimination : t -> t
(** [apply_conjunction_elimination st] applies conjunction elimination to all
    conjunctions, deriving their components. *)

val apply_disjunction_introduction : t -> t
(** [apply_disjunction_introduction st] applies disjunction introduction. *)

val apply_hypothetical_syllogism : t -> t
(** [apply_hypothetical_syllogism st] applies hypothetical syllogism. *)

val apply_contraposition : t -> t
(** [apply_contraposition st] applies contraposition to all implications. *)

val apply_disjunction_elimination : t -> t
(** [apply_disjunction_elimination st] applies disjunction elimination. *)

val apply_biconditional_introduction : t -> t
(** [apply_biconditional_introduction st] applies biconditional introduction. *)

val apply_biconditional_elimination : t -> t
(** [apply_biconditional_elimination st] applies biconditional elimination. *)

val apply_negation_introduction : t -> t
(** [apply_negation_introduction st] applies negation introduction. *)

val apply_double_negation_introduction : t -> t
(** [apply_double_negation_introduction st] applies double negation introduction. *)

val apply_double_negation_elimination : t -> t
(** [apply_double_negation_elimination st] applies double negation elimination. *)

val get_all_formulas : t -> prop list
(** [get_all_formulas st] returns all formulas in the state. *)

val count_formulas : t -> int
(** [count_formulas st] returns the total number of formulas. *)

val has_formula : t -> prop -> bool
(** [has_formula st p] returns true if [p] appears in the state. *)

val remove_premise : t -> prop -> t
(** [remove_premise st p] removes [p] from premises. *)

val clear_derived : t -> t
(** [clear_derived st] clears all derived formulas. *)

val clear_goal : t -> t
(** [clear_goal st] clears the goal. *)

val get_premises : t -> prop list
(** [get_premises st] returns the list of premises. *)

val get_derived : t -> prop list
(** [get_derived st] returns the list of derived formulas. *)

val get_goal : t -> prop option
(** [get_goal st] returns the goal if it exists. *)

val is_empty : t -> bool
(** [is_empty st] returns true if the state is empty. *)

val get_statistics : t -> int * int * bool * bool
(** [get_statistics st] returns (premise_count, derived_count, goal_set, goal_reached). *)

val apply_all_rules : t -> t
(** [apply_all_rules st] applies all inference rules exhaustively. *)

val find_derivations : t -> prop -> (string * prop * prop) list
(** [find_derivations st p] finds possible ways to derive [p]. *)

val export_state : t -> string
(** [export_state st] returns a string representation of the state. *)

val filter_redundant_derived : prop list -> prop list
(** [filter_redundant_derived derived] filters out redundant formulas from the
    derived list, removing duplicates and overly complex formulas. *)
