open OUnit2
open Ast
open Parser
open Sequent
open Rule

let assert_parse_error name input =
  match parse_prop input with
  | exception Parser.Parse_error _ -> ()
  | _ -> assert_failure (name ^ ": expected parse error for " ^ input)

(* ========== AST Tests ========== *)
let ast_tests =
  "ast"
  >::: [
         ( "prop_to_string var" >:: fun _ ->
           assert_equal ~printer:(fun x -> x) "A" (prop_to_string (Var "A")) );
         ( "prop_to_string and" >:: fun _ ->
           assert_equal
             ~printer:(fun x -> x)
             "(A & B)"
             (prop_to_string (And (Var "A", Var "B"))) );
         ( "prop_to_string implication" >:: fun _ ->
           assert_equal
             ~printer:(fun x -> x)
             "(A -> B)"
             (prop_to_string (Imp (Var "A", Var "B"))) );
         ( "prop_to_string negation" >:: fun _ ->
           assert_equal
             ~printer:(fun x -> x)
             "!A"
             (prop_to_string (Not (Var "A"))) );
         ( "prop_to_string nested" >:: fun _ ->
           assert_equal
             ~printer:(fun x -> x)
             "(!A & (B -> C))"
             (prop_to_string (And (Not (Var "A"), Imp (Var "B", Var "C")))) );
         ( "prop_to_string or" >:: fun _ ->
           assert_equal
             ~printer:(fun x -> x)
             "(A | B)"
             (prop_to_string (Or (Var "A", Var "B"))) );
         ( "prop_to_string or nested" >:: fun _ ->
           assert_equal
             ~printer:(fun x -> x)
             "((A | B) & C)"
             (prop_to_string (And (Or (Var "A", Var "B"), Var "C"))) );
       ]

(* ========== Parser Tests ========== *)
let parser_tests =
  "parser"
  >::: [
         ( "simple var" >:: fun _ ->
           assert_equal ~printer:prop_to_string (Var "A") (parse_prop "A") );
         ( "conjunction precedence" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (And (Var "A", Var "B"))
             (parse_prop "A & B") );
         ( "implication binds last" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Imp (And (Var "A", Var "B"), Var "C"))
             (parse_prop "(A & B) -> C") );
         ( "negation parentheses" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Not (Imp (Var "A", Var "B")))
             (parse_prop "!(A -> B)") );
         ( "parse with outer parentheses" >:: fun _ ->
           assert_equal ~printer:prop_to_string (Var "A") (parse_prop "(A)") );
         ( "parse nested parentheses" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (And (Var "A", Var "B"))
             (parse_prop "((A & B))") );
         ( "parse complex nested" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Imp (And (Var "A", Var "B"), Imp (Var "C", Var "D")))
             (parse_prop "(A & B) -> (C -> D)") );
         ( "parse negation of var" >:: fun _ ->
           assert_equal ~printer:prop_to_string (Not (Var "A"))
             (parse_prop "!A") );
         ( "parse negation of complex" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Not (And (Var "A", Var "B")))
             (parse_prop "!(A & B)") );
         ( "parse simple disjunction" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Or (Var "A", Var "B"))
             (parse_prop "A | B") );
         ( "parse disjunction precedence with and" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (And (Var "A", Or (Var "B", Var "C")))
             (parse_prop "A & B | C") );
         ( "parse disjunction precedence with implication" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Imp (Or (Var "A", Var "B"), Var "C"))
             (parse_prop "(A | B) -> C") );
         ( "parse disjunction with parentheses" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (And (Or (Var "A", Var "B"), Var "C"))
             (parse_prop "(A | B) & C") );
         ( "parse negation of disjunction" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Not (Or (Var "A", Var "B")))
             (parse_prop "!(A | B)") );
         ( "parse nested disjunction" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Or (Var "A", Or (Var "B", Var "C")))
             (parse_prop "A | B | C") );
         ( "parse complex with disjunction" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (Imp (Var "A", Or (Var "B", Var "C")))
             (parse_prop "A -> B | C") );
         ( "reject incomplete or" >:: fun _ ->
           assert_parse_error "incomplete or" "A |" );
         ("reject lowercase var" >:: fun _ -> assert_parse_error "lowercase" "a");
         ("reject empty" >:: fun _ -> assert_parse_error "empty" "");
         ( "reject invalid variable" >:: fun _ ->
           assert_parse_error "invalid var" "1" );
         ( "reject unmatched paren" >:: fun _ ->
           assert_parse_error "unmatched" "(A" );
         ( "reject unmatched closing paren" >:: fun _ ->
           assert_parse_error "unmatched close" "A)" );
         ( "reject incomplete implication" >:: fun _ ->
           assert_parse_error "incomplete imp" "A ->" );
         ( "reject incomplete and" >:: fun _ ->
           assert_parse_error "incomplete and" "A &" );
       ]

(* ========== Rule Tests ========== *)
let rule_tests =
  "rule"
  >::: [
         (* === Modus Tollens Tests === *)
         ( "modus_tollens basic" >:: fun _ ->
           let a = Var "A" in
           let b = Var "B" in
           let result = modus_tollens (Not b) (Imp (a, b)) in
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (Not a)) result );
         ( "modus_tollens reversed order" >:: fun _ ->
           let a = Var "A" in
           let b = Var "B" in
           let result = modus_tollens (Imp (a, b)) (Not b) in
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (Not a)) result );
         ( "modus_tollens no_match" >:: fun _ ->
           let a = Var "A" in
           let b = Var "B" in
           let result = modus_tollens a b in
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             None result );
         (* === Modus Ponens Tests === *)
         ( "modus_ponens forward" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (Var "B"))
             (modus_ponens (Var "A") (Imp (Var "A", Var "B"))) );
         ( "modus_ponens backward" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (Var "B"))
             (modus_ponens (Imp (Var "A", Var "B")) (Var "A")) );
         ( "modus_ponens no match" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             None
             (modus_ponens (Var "A") (Var "B")) );
         ( "modus_ponens wrong premise" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             None
             (modus_ponens (Var "C") (Imp (Var "A", Var "B"))) );
         ( "modus_ponens complex" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (And (Var "B", Var "C")))
             (modus_ponens (Var "A") (Imp (Var "A", And (Var "B", Var "C")))) );
         ( "conjunction_introduction simple" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (And (Var "A", Var "B")))
             (conjunction_introduction (Var "A") (Var "B")) );
         ( "conjunction_introduction complex" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (And (Var "A", Imp (Var "B", Var "C"))))
             (conjunction_introduction (Var "A") (Imp (Var "B", Var "C"))) );
         ( "conjunction_introduction nested" >:: fun _ ->
           assert_equal
             ~printer:(function
               | Some p -> prop_to_string p
               | None -> "None")
             (Some (And (And (Var "A", Var "B"), Var "C")))
             (conjunction_introduction (And (Var "A", Var "B")) (Var "C")) );
         ( "conjunction_introduction always succeeds" >:: fun _ ->
           match conjunction_introduction (Var "A") (Var "B") with
           | Some _ -> ()
           | None -> assert_failure "Conjunction introduction should always succeed" );
       ]

(* ========== Sequent Tests ========== *)
let sequent_tests =
  "sequent"
  >::: [
         ( "empty state" >:: fun _ ->
           assert_equal [] empty.premises;
           assert_equal [] empty.derived;
           assert_equal None empty.goal );
         ( "add_premise" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           assert_equal [ Var "A" ] st.premises;
           assert_equal [] st.derived;
           assert_equal None st.goal );
         ( "add_premise multiple" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s -> add_premise s (Var "B")
           in
           assert_bool "Should contain A" (List.mem (Var "A") st.premises);
           assert_bool "Should contain B" (List.mem (Var "B") st.premises) );
         ( "add_derived" >:: fun _ ->
           let st = add_derived empty (Var "A") in
           assert_equal [] st.premises;
           assert_equal [ Var "A" ] st.derived;
           assert_equal None st.goal );
         ( "add_derived prevents duplicate" >:: fun _ ->
           let st = add_derived empty (Var "A") in
           let st2 = add_derived st (Var "A") in
           assert_equal 1 (List.length st2.derived) );
         ( "add_derived prevents if in premises" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           let st2 = add_derived st (Var "A") in
           assert_equal [] st2.derived;
           assert_bool "Should still be in premises"
             (List.mem (Var "A") st2.premises) );
         ( "add_goal" >:: fun _ ->
           let st = add_goal empty (Var "A") in
           assert_equal (Some (Var "A")) st.goal );
         ( "apply_modus_ponens simple" >:: fun _ ->
           let st1 = add_premise empty (Var "A") in
           let st = add_premise st1 (Imp (Var "A", Var "B")) in
           let result = apply_modus_ponens st in
           assert_bool "Should derive B" (List.mem (Var "B") result.derived) );
         ( "apply_modus_ponens multiple steps" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", Var "B")) |> fun s ->
             add_premise s (Imp (Var "B", Var "C"))
           in
           let result = apply_modus_ponens st in
           assert_bool "Should derive B" (List.mem (Var "B") result.derived);
           assert_bool "Should derive C" (List.mem (Var "C") result.derived) );
         ( "apply_modus_ponens no new derivations" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           let result = apply_modus_ponens st in
           assert_equal [] result.derived );
         ( "apply_modus_ponens bidirectional" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "B") |> fun s ->
             add_premise s (Imp (Var "B", Var "A"))
           in
           let result = apply_modus_ponens st in
           assert_bool "Should derive A from B -> A and B"
             (List.mem (Var "A") result.derived) );
         ( "judge_goal true when in premises" >:: fun _ ->
           let st =
             add_premise empty (Var "A") |> fun s -> add_goal s (Var "A")
           in
           assert_bool "Goal should be judged true" (judge_goal st) );
         ( "judge_goal true when in derived" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", Var "B")) |> fun s ->
             add_goal s (Var "B")
           in
           let st = apply_modus_ponens st in
           assert_bool "Goal should be judged true" (judge_goal st) );
         ( "judge_goal false when not found" >:: fun _ ->
           let st =
             add_premise empty (Var "A") |> fun s -> add_goal s (Var "B")
           in
           assert_bool "Goal should be judged false" (not (judge_goal st)) );
         ( "judge_goal false when no goal" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           assert_bool "Goal should be judged false" (not (judge_goal st)) );
         ( "explain_derivation found" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", Var "B"))
           in
           let st = apply_modus_ponens st in
           match explain_derivation st (Var "B") with
           | Some (a, imp) ->
               assert_equal (Var "A") a;
               assert_equal (Imp (Var "A", Var "B")) imp
           | None -> assert_failure "Should find derivation" );
         ( "explain_derivation not found" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           match explain_derivation st (Var "B") with
           | Some _ -> assert_failure "Should not find derivation"
           | None -> () );
         ( "explain_derivation complex" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", And (Var "B", Var "C")))
           in
           let st = apply_modus_ponens st in
           match explain_derivation st (And (Var "B", Var "C")) with
           | Some (a, imp) ->
               assert_equal (Var "A") a;
               assert_equal (Imp (Var "A", And (Var "B", Var "C"))) imp
           | None -> assert_failure "Should find derivation" );
         ( "apply_conjunction_introduction simple" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s -> add_premise s (Var "B")
           in
           let result = apply_conjunction_introduction st in
           assert_bool "Should derive A & B"
             (List.mem (And (Var "A", Var "B")) result.derived) );
         ( "apply_conjunction_introduction multiple pairs" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Var "B") |> fun s -> add_premise s (Var "C")
           in
           let result = apply_conjunction_introduction st in
           assert_bool "Should derive A & B"
             (List.mem (And (Var "A", Var "B")) result.derived);
           assert_bool "Should derive A & C"
             (List.mem (And (Var "A", Var "C")) result.derived);
           assert_bool "Should derive B & C"
             (List.mem (And (Var "B", Var "C")) result.derived) );
         ( "apply_conjunction_introduction no self-conjunction" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           let result = apply_conjunction_introduction st in
           (* Should not derive A & A since we skip when p1 = p2 *)
           assert_bool "Should not derive A & A"
             (not (List.mem (And (Var "A", Var "A")) result.derived)) );
         ( "apply_conjunction_introduction with derived props" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", Var "B")) |> fun s ->
             apply_modus_ponens s
           in
           (* Now we have A, A -> B in premises, and B in derived *)
           let result = apply_conjunction_introduction st in
           assert_bool "Should derive A & B from premise and derived"
             (List.mem (And (Var "A", Var "B")) result.derived);
           assert_bool "Should derive A & (A -> B) from two premises"
             (List.mem (And (Var "A", Imp (Var "A", Var "B"))) result.derived);
           assert_bool "Should derive (A -> B) & B"
             (List.mem (And (Imp (Var "A", Var "B"), Var "B")) result.derived) );
         ( "apply_conjunction_introduction generates conjunctions" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s -> add_premise s (Var "B")
           in
           let result = apply_conjunction_introduction st in
           (* Should have A & B in derived *)
           assert_bool "Should derive A & B"
             (List.exists
                (function
                  | And (Var "A", Var "B") -> true
                  | _ -> false)
                result.derived) );
         ( "apply_conjunction_introduction no new derivations when none possible" >:: fun _ ->
           let st = add_premise empty (Var "A") in
           let result = apply_conjunction_introduction st in
           assert_equal [] result.derived );
       ]

(* ========== Simplify Tests ========== *)
let simplify_tests =
  "simplify"
  >::: [
         ( "simplify !!A → A" >:: fun _ ->
           assert_equal ~printer:prop_to_string (Var "A")
             (simplify (Not (Not (Var "A")))) );
         ( "simplify !!!A → !A" >:: fun _ ->
           assert_equal ~printer:prop_to_string (Not (Var "A"))
             (simplify (Not (Not (Not (Var "A"))))) );
         ( "simplify !!!!A → A" >:: fun _ ->
           assert_equal ~printer:prop_to_string (Var "A")
             (simplify (Not (Not (Not (Not (Var "A")))))) );
         ( "simplify (A & !!B) → (A & B)" >:: fun _ ->
           assert_equal ~printer:prop_to_string
             (And (Var "A", Var "B"))
             (simplify (And (Var "A", Not (Not (Var "B"))))) );
         ( "simplify (!!A -> !(!B)) → (A -> !B)" >:: fun _ ->
           let input = Imp (Not (Not (Var "A")), Not (Not (Not (Var "B")))) in
           let expected = Imp (Var "A", Not (Var "B")) in
           assert_equal ~printer:prop_to_string expected (simplify input) );
         ( "simplify !(!!(!!A)) → A" >:: fun _ ->
           let input = Not (Not (Not (Not (Var "A")))) in
           let expected = Var "A" in
           assert_equal ~printer:prop_to_string expected (simplify input) );
         ( "simplify (!!A & !!B) → (A & B)" >:: fun _ ->
           let input = And (Not (Not (Var "A")), Not (Not (Var "B"))) in
           let expected = And (Var "A", Var "B") in
           assert_equal ~printer:prop_to_string expected (simplify input) );
       ]

(* ========== AST Utility Tests ========== *)
let ast_utility_tests =
  "ast_utility"
  >::: [
         ( "get_variables simple" >:: fun _ ->
           let vars = get_variables (Var "A") in
           assert_equal ~printer:(fun xs -> String.concat "," xs) [ "A" ] vars );
         ( "get_variables complex" >:: fun _ ->
           let vars = get_variables (And (Var "A", Imp (Var "B", Var "C"))) in
           let sorted = List.sort compare vars in
           assert_equal ~printer:(fun xs -> String.concat "," xs)
             [ "A"; "B"; "C" ] sorted );
         ( "depth atomic" >:: fun _ -> assert_equal 0 (depth (Var "A")) );
         ( "depth simple" >:: fun _ ->
           assert_equal 1 (depth (And (Var "A", Var "B"))) );
         ( "depth nested" >:: fun _ ->
           assert_equal 2
             (depth (And (Var "A", Imp (Var "B", Var "C")))) );
         ( "size atomic" >:: fun _ -> assert_equal 1 (size (Var "A")) );
         ( "size binary" >:: fun _ ->
           assert_equal 3 (size (And (Var "A", Var "B"))) );
         ( "is_atomic true" >:: fun _ -> assert_bool "Should be atomic" (is_atomic (Var "A")) );
         ( "is_atomic false" >:: fun _ ->
           assert_bool "Should not be atomic" (not (is_atomic (And (Var "A", Var "B")))) );
         ( "is_negation true" >:: fun _ ->
           assert_bool "Should be negation" (is_negation (Not (Var "A"))) );
         ( "is_negation false" >:: fun _ ->
           assert_bool "Should not be negation" (not (is_negation (Var "A"))) );
         ( "is_conjunction true" >:: fun _ ->
           assert_bool "Should be conjunction"
             (is_conjunction (And (Var "A", Var "B"))) );
         ( "is_conjunction false" >:: fun _ ->
           assert_bool "Should not be conjunction" (not (is_conjunction (Var "A"))) );
         ( "is_disjunction true" >:: fun _ ->
           assert_bool "Should be disjunction" (is_disjunction (Or (Var "A", Var "B"))) );
         ( "is_implication true" >:: fun _ ->
           assert_bool "Should be implication"
             (is_implication (Imp (Var "A", Var "B"))) );
         ( "count_operators simple" >:: fun _ ->
           let a, o, i, n = count_operators (And (Var "A", Var "B")) in
           assert_equal 1 a;
           assert_equal 0 o;
           assert_equal 0 i;
           assert_equal 0 n );
         ( "count_operators complex" >:: fun _ ->
           let a, o, i, n =
             count_operators (Imp (And (Var "A", Var "B"), Not (Var "C")))
           in
           assert_equal 1 a;
           assert_equal 0 o;
           assert_equal 1 i;
           assert_equal 1 n );
         ( "subformulas atomic" >:: fun _ ->
           let subs = subformulas (Var "A") in
           assert_equal 1 (List.length subs);
           assert_bool "Should contain A" (List.mem (Var "A") subs) );
         ( "subformulas complex" >:: fun _ ->
           let p = And (Var "A", Var "B") in
           let subs = subformulas p in
           assert_bool "Should contain itself" (List.mem p subs);
           assert_bool "Should contain A" (List.mem (Var "A") subs);
           assert_bool "Should contain B" (List.mem (Var "B") subs) );
         ( "contains true" >:: fun _ ->
           assert_bool "Should contain subformula"
             (contains (And (Var "A", Var "B")) (Var "A")) );
         ( "contains false" >:: fun _ ->
           assert_bool "Should not contain"
             (not (contains (Var "A") (Var "B"))) );
         ( "get_left_operand" >:: fun _ ->
           match get_left_operand (And (Var "A", Var "B")) with
           | Some p -> assert_equal (Var "A") p
           | None -> assert_failure "Should have left operand" );
         ( "get_right_operand" >:: fun _ ->
           match get_right_operand (Imp (Var "A", Var "B")) with
           | Some p -> assert_equal (Var "B") p
           | None -> assert_failure "Should have right operand" );
         ( "get_negated_formula" >:: fun _ ->
           match get_negated_formula (Not (Var "A")) with
           | Some p -> assert_equal (Var "A") p
           | None -> assert_failure "Should have negated formula" );
         ( "get_antecedent" >:: fun _ ->
           match get_antecedent (Imp (Var "A", Var "B")) with
           | Some p -> assert_equal (Var "A") p
           | None -> assert_failure "Should have antecedent" );
         ( "get_consequent" >:: fun _ ->
           match get_consequent (Imp (Var "A", Var "B")) with
           | Some p -> assert_equal (Var "B") p
           | None -> assert_failure "Should have consequent" );
       ]

(* ========== Additional Rule Tests ========== *)
let additional_rule_tests =
  "additional_rules"
  >::: [
         ( "conjunction_elimination_left" >:: fun _ ->
           match conjunction_elimination_left (And (Var "A", Var "B")) with
           | Some p -> assert_equal (Var "A") p
           | None -> assert_failure "Should eliminate left" );
         ( "conjunction_elimination_right" >:: fun _ ->
           match conjunction_elimination_right (And (Var "A", Var "B")) with
           | Some p -> assert_equal (Var "B") p
           | None -> assert_failure "Should eliminate right" );
         ( "disjunction_introduction_left" >:: fun _ ->
           match disjunction_introduction_left (Var "A") (Var "B") with
           | Some p -> assert_equal (Or (Var "A", Var "B")) p
           | None -> assert_failure "Should introduce disjunction" );
         ( "hypothetical_syllogism" >:: fun _ ->
           match
             hypothetical_syllogism (Imp (Var "A", Var "B"))
               (Imp (Var "B", Var "C"))
           with
           | Some p -> assert_equal (Imp (Var "A", Var "C")) p
           | None -> assert_failure "Should apply hypothetical syllogism" );
         ( "contraposition" >:: fun _ ->
           match contraposition (Imp (Var "A", Var "B")) with
           | Some p -> assert_equal (Imp (Not (Var "B"), Not (Var "A"))) p
           | None -> assert_failure "Should apply contraposition" );
         ( "double_negation_introduction" >:: fun _ ->
           match double_negation_introduction (Var "A") with
           | Some p -> assert_equal (Not (Not (Var "A"))) p
           | None -> assert_failure "Should introduce double negation" );
         ( "double_negation_elimination" >:: fun _ ->
           match double_negation_elimination (Not (Not (Var "A"))) with
           | Some p -> assert_equal (Var "A") p
           | None -> assert_failure "Should eliminate double negation" );
         ( "exportation" >:: fun _ ->
           match exportation (Imp (And (Var "A", Var "B"), Var "C")) with
           | Some p ->
               assert_equal (Imp (Var "A", Imp (Var "B", Var "C"))) p
           | None -> assert_failure "Should apply exportation" );
         ( "importation" >:: fun _ ->
           match importation (Imp (Var "A", Imp (Var "B", Var "C"))) with
           | Some p -> assert_equal (Imp (And (Var "A", Var "B"), Var "C")) p
           | None -> assert_failure "Should apply importation" );
       ]

(* ========== Additional Sequent Tests ========== *)
let additional_sequent_tests =
  "additional_sequent"
  >::: [
         ( "apply_conjunction_elimination" >:: fun _ ->
           let st =
             empty |> fun s -> add_premise s (And (Var "A", Var "B"))
           in
           let result = apply_conjunction_elimination st in
           assert_bool "Should derive A" (List.mem (Var "A") result.derived);
           assert_bool "Should derive B" (List.mem (Var "B") result.derived) );
         ( "apply_hypothetical_syllogism" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Imp (Var "A", Var "B")) |> fun s ->
             add_premise s (Imp (Var "B", Var "C"))
           in
           let result = apply_hypothetical_syllogism st in
           assert_bool "Should derive A -> C"
             (List.mem (Imp (Var "A", Var "C")) result.derived) );
         ( "apply_contraposition" >:: fun _ ->
           let st = empty |> fun s -> add_premise s (Imp (Var "A", Var "B")) in
           let result = apply_contraposition st in
           assert_bool "Should derive !B -> !A"
             (List.mem (Imp (Not (Var "B"), Not (Var "A"))) result.derived) );
         ( "get_all_formulas" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s -> add_premise s (Var "B")
           in
           let all = get_all_formulas st in
           assert_bool "Should contain A" (List.mem (Var "A") all);
           assert_bool "Should contain B" (List.mem (Var "B") all) );
         ( "count_formulas" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s -> add_premise s (Var "B")
           in
           assert_equal 2 (count_formulas st) );
         ( "has_formula true" >:: fun _ ->
           let st = empty |> fun s -> add_premise s (Var "A") in
           assert_bool "Should have A" (has_formula st (Var "A")) );
         ( "has_formula false" >:: fun _ ->
           let st = empty |> fun s -> add_premise s (Var "A") in
           assert_bool "Should not have B" (not (has_formula st (Var "B"))) );
         ( "remove_premise" >:: fun _ ->
           let st = empty |> fun s -> add_premise s (Var "A") in
           let st' = remove_premise st (Var "A") in
           assert_equal [] st'.premises );
         ( "clear_derived" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", Var "B")) |> fun s ->
             apply_modus_ponens s
           in
           let st' = clear_derived st in
           assert_equal [] st'.derived );
         ( "clear_goal" >:: fun _ ->
           let st = empty |> fun s -> add_goal s (Var "A") in
           let st' = clear_goal st in
           assert_equal None st'.goal );
         ( "is_empty true" >:: fun _ ->
           assert_bool "Should be empty" (is_empty empty) );
         ( "is_empty false" >:: fun _ ->
           let st = empty |> fun s -> add_premise s (Var "A") in
           assert_bool "Should not be empty" (not (is_empty st)) );
         ( "get_statistics" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s -> add_goal s (Var "A")
           in
           let prem_count, deriv_count, goal_set, goal_reached =
             get_statistics st
           in
           assert_equal 1 prem_count;
           assert_equal 0 deriv_count;
           assert_bool "Goal should be set" goal_set;
           assert_bool "Goal should be reached" goal_reached );
         ( "find_derivations found" >:: fun _ ->
           let st =
             empty |> fun s ->
             add_premise s (Var "A") |> fun s ->
             add_premise s (Imp (Var "A", Var "B"))
           in
           let derivs = find_derivations st (Var "B") in
           assert_bool "Should find derivations" (derivs <> []) );
         ( "find_derivations not found" >:: fun _ ->
           let st = empty |> fun s -> add_premise s (Var "A") in
           let derivs = find_derivations st (Var "B") in
           assert_bool "Should not find derivations" (derivs = []) );
       ]

let suite =
  "logic prover tests"
  >::: [
         ast_tests;
         parser_tests;
         rule_tests;
         sequent_tests;
         simplify_tests;
         ast_utility_tests;
         additional_rule_tests;
         additional_sequent_tests;
       ]

let () = run_test_tt_main suite
