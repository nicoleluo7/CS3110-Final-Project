(* Abstract syntax tree (AST) for propositional logic formulas. This is the core
   data type that represents formulas internally. *)
type prop =
  | Var of string (* A single propositional variable, e.g. "A" *)
  | And of prop * prop (* Conjunction: p & q *)
  | Imp of prop * prop (* Implication: p -> q *)
  | Not of prop (* Negation: !p *)

(* Convert a prop back into a readable string. Used for printing formulas in the
   REPL and later in the proof interface. *)
let rec prop_to_string = function
  | Var p -> p
  | And (p1, p2) -> "(" ^ prop_to_string p1 ^ " & " ^ prop_to_string p2 ^ ")"
  | Imp (p1, p2) -> "(" ^ prop_to_string p1 ^ " -> " ^ prop_to_string p2 ^ ")"
  | Not p -> "!" ^ prop_to_string p
