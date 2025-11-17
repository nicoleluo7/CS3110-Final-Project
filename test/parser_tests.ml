open OUnit2
open Ast
open Parser
open Sequent

let assert_parse_error name input =
  match parse_prop input with
  | exception Parser.Parse_error _ -> ()
  | _ -> assert_failure (name ^ ": expected parse error for " ^ input)

let parser_tests =
  "parser"
  >::: [
         "simple var"
         >:: (fun _ ->
               assert_equal ~printer:prop_to_string (Var "A") (parse_prop "A"));
         "conjunction precedence"
         >:: (fun _ ->
               assert_equal ~printer:prop_to_string
                 (And (Var "A", Var "B"))
                 (parse_prop "A & B"));
         "implication binds last"
         >:: (fun _ ->
               assert_equal ~printer:prop_to_string
                 (Imp (And (Var "A", Var "B"), Var "C"))
                 (parse_prop "(A & B) -> C"));
         "negation parentheses"
         >:: (fun _ ->
               assert_equal ~printer:prop_to_string
                 (Not (Imp (Var "A", Var "B")))
                 (parse_prop "!(A -> B)"));
         "reject lowercase var"
         >:: (fun _ -> assert_parse_error "lowercase" "a");
         "reject empty" >:: (fun _ -> assert_parse_error "empty" "")
       ]

let modus_ponens_test =
  "modus ponens"
  >:: fun _ ->
  let st1 = add_premise empty (Var "A") in
  let st = add_premise st1 (Imp (Var "A", Var "B")) in
  let derived = (apply_modus_ponens st).derived in
  assert_bool "Should derive B" (List.mem (Var "B") derived)

let suite = "logic prover tests" >::: [parser_tests; modus_ponens_test]

let () = run_test_tt_main suite

