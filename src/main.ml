open Ast
open Parser
open Sequent

type command =
  | AddPremise of prop
  | SetGoal of prop
  | Derive
  | Show
  | Reset
  | Help
  | Quit

let rec find_first_space s idx =
  if idx >= String.length s then None
  else if s.[idx] = ' ' then Some idx
  else find_first_space s (idx + 1)

let split_command s =
  match find_first_space s 0 with
  | None -> (s, "")
  | Some idx ->
      let cmd = String.sub s 0 idx in
      let rest = String.sub s (idx + 1) (String.length s - idx - 1) in
      (cmd, String.trim rest)

let parse_command line =
  let trimmed = String.trim line in
  if trimmed = "" then Error "Please enter a command."
  else
    let cmd, arg = split_command trimmed in
    match String.lowercase_ascii cmd with
    | "premise" | "p" ->
        if arg = "" then Error "Usage: premise <formula>"
        else (
          try Ok (AddPremise (parse_prop arg))
          with Parse_error msg -> Error ("Parse error: " ^ msg))
    | "goal" | "g" ->
        if arg = "" then Error "Usage: goal <formula>"
        else (
          try Ok (SetGoal (parse_prop arg))
          with Parse_error msg -> Error ("Parse error: " ^ msg))
    | "derive" | "d" -> Ok Derive
    | "show" | "state" | "s" -> Ok Show
    | "reset" | "clear" -> Ok Reset
    | "help" | "h" | "?" -> Ok Help
    | "quit" | "q" -> Ok Quit
    | unknown -> Error ("Unknown command: " ^ unknown)

let print_help () =
  print_endline "Commands:";
  print_endline "  premise <formula>  Add a premise to the proof state";
  print_endline "  goal <formula>     Set the current goal";
  print_endline "  derive             Apply Modus Ponens to derive new facts";
  print_endline "  show               Display the current proof state";
  print_endline "  reset              Clear all premises, derived facts, and goal";
  print_endline "  help               Show this message";
  print_endline "  quit               Exit the REPL"

let apply_and_show st =
  let st' = apply_modus_ponens st in
  print_result st';
  st'

let rec loop st =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file ->
      print_endline "\nExiting.";
      ()
  | line -> (
      match parse_command line with
      | Error msg ->
          print_endline ("Error: " ^ msg);
          loop st
      | Ok Quit ->
          print_endline "Goodbye!";
          ()
      | Ok Help ->
          print_help ();
          loop st
      | Ok Reset ->
          let st' = empty in
          print_endline "Proof state cleared.";
          loop st'
      | Ok Show ->
          print_result st;
          loop st
      | Ok Derive ->
          let st' = apply_and_show st in
          loop st'
      | Ok (AddPremise p) ->
          print_endline ("Added premise: " ^ prop_to_string p);
          let st' = add_premise st p |> apply_and_show in
          loop st'
      | Ok (SetGoal p) ->
          print_endline ("Set goal: " ^ prop_to_string p);
          let st' = add_goal st p |> apply_and_show in
          loop st')

let () =
  print_endline "Welcome to the Logic Prover demo.";
  print_help ();
  loop empty
