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

let suite =
  "logic prover tests"
  >::: [ ast_tests; parser_tests; rule_tests; sequent_tests; simplify_tests ]

let () = run_test_tt_main suite
