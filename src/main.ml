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
  | Shortcuts
  | Load of string

(* -------------------------------------------------------------------------- *)
let print_state st =
  print_endline "";
  print_endline "──────────────────────────────";
  print_endline " PROOF STATE";
  print_endline "──────────────────────────────";

  print_endline "Premises:";
  (match st.premises with
  | [] -> print_endline "  (none)"
  | _ ->
      List.iteri
        (fun i p -> Printf.printf "  %d. %s\n" (i + 1) (prop_to_string p))
        st.premises);

  print_endline "\nDerived:";
  (match st.derived with
  | [] -> print_endline "  (none)"
  | _ ->
      List.iter
        (fun p -> Printf.printf "  - %s\n" (prop_to_string p))
        st.derived);

  print_endline "\nGoal:";
  (match st.goal with
  | None -> print_endline "  (none)"
  | Some g ->
      let status = if judge_goal st then "✅ reached" else "❌ not reached" in
      Printf.printf "  %s  %s\n" (prop_to_string g) status);

  print_endline "──────────────────────────────\n"

(* -------------------------------------------------------------------------- *)
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

(* -------------------------------------------------------------------------- *)
(* read multiline: used ONLY for formulas (p <formula> etc) *)
let rec read_multiline acc =
  let prompt = if acc = [] then "logic> " else "....> " in
  print_string prompt;
  flush stdout;
  match try Some (read_line ()) with End_of_file -> None with
  | None -> None
  | Some line ->
      let trimmed = String.trim line in
      if trimmed = "" && acc <> [] then Some (String.concat "\n" (List.rev acc))
      else if trimmed = ";;" then Some (String.concat "\n" (List.rev acc))
      else read_multiline (line :: acc)

(* -------------------------------------------------------------------------- *)
let parse_command line =
  let trimmed = String.trim line in
  if trimmed = "" then Error "Please enter a command."
  else
    let cmd, arg = split_command trimmed in
    match String.lowercase_ascii cmd with
    | "premise" | "p" -> (
        if arg = "" then Error "Usage: premise <formula>"
        else
          try Ok (AddPremise (parse_prop arg))
          with Parse_error msg -> Error ("Parse error: " ^ msg))
    | "goal" | "g" -> (
        if arg = "" then Error "Usage: goal <formula>"
        else
          try Ok (SetGoal (parse_prop arg))
          with Parse_error msg -> Error ("Parse error: " ^ msg))
    | "load" ->
        if arg = "" then Error "Usage: load <filename>" else Ok (Load arg)
    | "derive" | "d" -> Ok Derive
    | "show" | "state" | "s" -> Ok Show
    | "reset" | "clear" -> Ok Reset
    | "help" | "h" | "?" -> Ok Help
    | "quit" | "q" -> Ok Quit
    | "shortcuts" -> Ok Shortcuts
    | unknown -> Error ("Unknown command: " ^ unknown)

(* -------------------------------------------------------------------------- *)
let print_banner () =
  print_endline "===========================================";
  print_endline "       Propositional Logic REPL v1.0";
  print_endline "===========================================";
  print_endline ""

let print_shortcuts () =
  print_endline "──────────────────────────────";
  print_endline " Command Shortcuts:";
  print_endline "──────────────────────────────";
  print_endline "  p           premise";
  print_endline "  g           goal";
  print_endline "  d           derive";
  print_endline "  s           show";
  print_endline "  h, ?        help";
  print_endline "  q           quit";
  print_endline "──────────────────────────────\n"

let print_help () =
  print_endline "──────────────────────────────";
  print_endline " Commands:";
  print_endline "──────────────────────────────";
  print_endline "  premise <formula>   Add a premise";
  print_endline "  goal <formula>      Set the goal";
  print_endline "  derive              Apply Modus Ponens";
  print_endline "  show                Show current state";
  print_endline "  reset               Clear proof state";
  print_endline "  load <file>         Load premises/goals from script";
  print_endline "  help                Show help message";
  print_endline "  shortcuts           Print command shortcuts";
  print_endline "  quit                Exit";
  print_endline "──────────────────────────────\n"

(* -------------------------------------------------------------------------- *)
let apply_and_show st =
  let before = st.derived in
  let st' = apply_modus_ponens st in
  let after = st'.derived in

  (* only show new derivations *)
  let new_items = List.filter (fun p -> not (List.mem p before)) after in

  List.iter
    (fun derived ->
      match Sequent.explain_derivation st derived with
      | Some (a, imp) ->
          Printf.printf "Derived: %s    (from %s and %s via Modus Ponens)\n"
            (prop_to_string derived) (prop_to_string a) (prop_to_string imp)
      | None -> Printf.printf "Derived: %s\n" (prop_to_string derived))
    new_items;

  (* always show full proof state *)
  print_state st';
  st'

(* -------------------------------------------------------------------------- *)
(* Execute commands inside a script file *)
let rec run_script st lines =
  match lines with
  | [] -> st
  | line :: rest -> (
      let line = String.trim line in
      if line = "" || line.[0] = '#' then run_script st rest
      else
        match parse_command line with
        | Error msg ->
            Printf.printf "Script error: %s\n" msg;
            run_script st rest
        | Ok Quit ->
            print_endline "Quit ignored inside script.";
            run_script st rest
        | Ok Help | Ok Show | Ok Shortcuts ->
            (* UI commands ignored in scripts *)
            run_script st rest
        | Ok Reset -> run_script empty rest
        | Ok Derive ->
            let st' = apply_and_show st in
            run_script st' rest
        | Ok (AddPremise p) ->
            let st' = add_premise st p |> apply_and_show in
            run_script st' rest
        | Ok (SetGoal p) ->
            let st' = add_goal st p |> apply_and_show in
            run_script st' rest
        | Ok (Load _) ->
            print_endline "Nested load not supported.";
            run_script st rest)

(* -------------------------------------------------------------------------- *)
let rec loop st =
  print_string "> ";
  flush stdout;

  match read_line () with
  | exception End_of_file ->
      print_endline "\nGoodbye!";
      exit 0
  | line -> (
      match parse_command line with
      | Ok Quit ->
          print_endline "Goodbye!";
          exit 0
      | Error msg ->
          print_endline ("Error: " ^ msg);
          loop st
      | Ok Help ->
          print_help ();
          loop st
      | Ok Show ->
          print_state st;
          loop st
      | Ok Reset ->
          print_endline "Proof state cleared.";
          loop empty
      | Ok Shortcuts ->
          print_shortcuts ();
          loop st
      | Ok Derive ->
          let st' = apply_and_show st in
          loop st'
      (* reject/success message handled in backend *)
      | Ok (AddPremise p) ->
          let st' = add_premise st p |> apply_and_show in
          loop st'
      | Ok (SetGoal p) ->
          print_endline ("Set goal: " ^ prop_to_string p);
          let st' = add_goal st p |> apply_and_show in
          loop st'
      | Ok (Load filename) ->
          if not (Sys.file_exists filename) then (
            Printf.printf "Error: file not found: %s\n" filename;
            loop st)
          else
            let ch = open_in filename in
            let rec read_all acc =
              match input_line ch with
              | line -> read_all (line :: acc)
              | exception End_of_file -> List.rev acc
            in
            let lines = read_all [] in
            close_in ch;

            print_endline ("Loading script: " ^ filename);
            let st' = run_script st lines in
            print_endline "File loaded!";
            loop st')

(* -------------------------------------------------------------------------- *)
let () =
  print_banner ();
  print_help ();
  loop empty
