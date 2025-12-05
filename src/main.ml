open Ast
open Parser
open Sequent

(** Commands that can be executed in the REPL. *)
type command =
  | AddPremise of prop
  | SetGoal of prop
  | Show
  | Reset
  | Help
  | Quit
  | Shortcuts
  | Load of string
  | Export of string
  | Stats
  | ApplyAll
  | FindDerivations of prop
  | ClearDerived
  | ClearGoal
  | RemovePremise of prop
  | Rules

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
      let filtered = Sequent.filter_redundant_derived st.derived in
      List.iter (fun p -> Printf.printf "  - %s\n" (prop_to_string p)) filtered);

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
    | "show" | "state" | "s" -> Ok Show
    | "reset" | "clear" -> Ok Reset
    | "help" | "h" | "?" -> Ok Help
    | "quit" | "q" -> Ok Quit
    | "shortcuts" -> Ok Shortcuts
    | "rules" -> Ok Rules
    | "export" ->
        if arg = "" then Error "Usage: export <filename>" else Ok (Export arg)
    | "stats" | "statistics" -> Ok Stats
    | "applyall" | "aa" -> Ok ApplyAll
    | "find" | "findderivations" -> (
        if arg = "" then Error "Usage: find <formula>"
        else
          try Ok (FindDerivations (parse_prop arg))
          with Parse_error msg -> Error ("Parse error: " ^ msg))
    | "clearderived" | "cd" -> Ok ClearDerived
    | "cleargoal" | "cg" -> Ok ClearGoal
    | "remove" | "rm" -> (
        if arg = "" then Error "Usage: remove <formula>"
        else
          try Ok (RemovePremise (parse_prop arg))
          with Parse_error msg -> Error ("Parse error: " ^ msg))
    | unknown -> Error ("Unknown command: " ^ unknown)

(* -------------------------------------------------------------------------- *)
let print_banner () =
  print_endline "===========================================";
  print_endline "       Propositional Logic REPL MS3";
  print_endline "===========================================";
  print_endline "";
  print_endline "About:";
  print_endline "  An interactive proof assistant for propositional logic.";
  print_endline "  Build proofs by adding premises, applying inference rules,";
  print_endline "  and tracking goals. Supports formulas with variables (A-Z),";
  print_endline "  conjunctions (&), disjunctions (|), implications (->),";
  print_endline "  and negations (!).";
  print_endline "";
  print_endline "Quick Start Example:";
  print_endline "  premise A";
  print_endline "  premise (A -> B)";
  print_endline "  goal B";
  print_endline "  show";
  print_endline ""

let print_shortcuts () =
  print_endline "──────────────────────────────";
  print_endline " Command Shortcuts:";
  print_endline "──────────────────────────────";
  print_endline "  p           premise";
  print_endline "  g           goal";
  print_endline "  s           show";
  print_endline "  aa          applyall";
  print_endline "  cd          clearderived";
  print_endline "  cg          cleargoal";
  print_endline "  h, ?        help";
  print_endline "  q           quit";
  print_endline "──────────────────────────────\n"

let print_rules () =
  print_endline "──────────────────────────────";
  print_endline " Supported Inference Rules:";
  print_endline "──────────────────────────────";
  print_endline "";
  print_endline " Basic Rules:";
  print_endline "  • Modus Ponens:          From A and A -> B, derive B";
  print_endline "  • Modus Tollens:          From !B and A -> B, derive !A";
  print_endline "  • Conjunction Intro:      From A and B, derive A & B";
  print_endline "  • Conjunction Elim (L/R): From A & B, derive A (or B)";
  print_endline "";
  print_endline " Advanced Rules:";
  print_endline
    "  • Hypothetical Syllogism: From A -> B and B -> C, derive A -> C";
  print_endline "  • Contraposition:         From A -> B, derive !B -> !A";
  print_endline "";
  print_endline "──────────────────────────────\n"

let print_help () =
  print_endline "──────────────────────────────";
  print_endline " Commands:";
  print_endline "──────────────────────────────";
  print_endline "";
  print_endline " Proof Construction:";
  print_endline "  premise <formula>   Add a premise (auto-applies basic rules)";
  print_endline "  goal <formula>      Set the goal (auto-applies basic rules)";
  print_endline "  applyall            Apply ALL inference rules exhaustively";
  print_endline "";
  print_endline " State Management:";
  print_endline "  show                Show current state";
  print_endline "  reset               Clear entire proof state (all data)";
  print_endline
    "  clearderived        Clear only derived formulas (keep premises)";
  print_endline
    "  cleargoal           Clear only the goal (keep premises/derived)";
  print_endline "  remove <formula>    Remove a specific premise";
  print_endline "";
  print_endline " Analysis & Discovery:";
  print_endline "  stats               Show proof statistics (counts & status)";
  print_endline "  find <formula>      Find possible ways to derive formula";
  print_endline "";
  print_endline " File Operations:";
  print_endline "  load <file>         Load premises/goals from script file";
  print_endline "  export <file>       Export current state to text file";
  print_endline "";
  print_endline " Help & Navigation:";
  print_endline "  help                Show help message";
  print_endline "  shortcuts           Print command shortcuts";
  print_endline "  rules               List all supported inference rules";
  print_endline "  quit                Exit";
  print_endline "──────────────────────────────\n"

(* -------------------------------------------------------------------------- *)
let apply_and_show st =
  let before = st.derived in
  (* Apply basic inference rules: MP, MT, HS, Conj Intro/Elim, and
     Contraposition *)
  let st' = apply_modus_ponens st in
  let st'' = apply_conjunction_introduction st' in
  let st''' = Sequent.apply_conjunction_elimination st'' in
  let st'''' = Sequent.apply_contraposition st''' in
  let after = st''''.derived in

  (* only show new derivations *)
  let new_items = List.filter (fun p -> not (List.mem p before)) after in

  let visible_new =
    new_items
    |> List.map normalize_conjunction
    |> List.filter is_visible_formula
  in

  List.iter
    (fun d ->
      match Sequent.find_derivations st'''' d with
      | (rule, p1, p2) :: _ ->
          let parent_str =
            if p2 = Not (Var "") then Printf.sprintf "%s" (prop_to_string p1)
            else
              Printf.sprintf "%s and %s" (prop_to_string p1) (prop_to_string p2)
          in
          Printf.printf "Derived: %s   (%s from %s)\n" (prop_to_string d) rule
            parent_str
      | [] -> Printf.printf "Derived: %s\n" (prop_to_string d))
    visible_new;

  (* always show full proof state *)
  print_state st'''';
  st''''

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
        | Ok Help | Ok Show | Ok Shortcuts | Ok Rules ->
            (* UI commands ignored in scripts *)
            run_script st rest
        | Ok Reset -> run_script empty rest
        | Ok (AddPremise p) ->
            let st' = add_premise st p |> apply_and_show in
            run_script st' rest
        | Ok (SetGoal p) ->
            let st' = add_goal st p |> apply_and_show in
            run_script st' rest
        | Ok (Load _) ->
            print_endline "Nested load not supported.";
            run_script st rest
        | Ok (Export _)
        | Ok Stats
        | Ok ApplyAll
        | Ok (FindDerivations _)
        | Ok ClearDerived
        | Ok ClearGoal
        | Ok (RemovePremise _) ->
            (* UI commands ignored in scripts *)
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
      | Ok Rules ->
          print_rules ();
          loop st
      (* reject/success message handled in backend *)
      | Ok (AddPremise p) ->
          let st_after = add_premise st p in
          if st_after == st then loop st
          else
            let st' = apply_and_show st_after in
            loop st'
      | Ok (SetGoal p) ->
          print_endline ("Set goal: " ^ prop_to_string p);
          let st' = add_goal st p |> apply_and_show in
          loop st'
      | Ok (Export filename) ->
          let content = Sequent.export_state st in
          (try
             let ch = open_out filename in
             output_string ch content;
             close_out ch;
             Printf.printf "State exported to %s\n" filename
           with Sys_error msg ->
             Printf.printf "Error: could not write to file %s: %s\n" filename
               msg);
          loop st
      | Ok Stats ->
          let prem_count, deriv_count, goal_set, goal_reached =
            Sequent.get_statistics st
          in
          print_endline "──────────────────────────────";
          print_endline " Current Proof Statistics:";
          print_endline "──────────────────────────────";
          Printf.printf "  Premises: %d\n" prem_count;
          Printf.printf "  Derived: %d\n" deriv_count;
          Printf.printf "  Goal set: %s\n" (if goal_set then "Yes" else "No");
          Printf.printf "  Goal reached: %s\n"
            (if goal_reached then "Yes" else "No");
          print_endline "──────────────────────────────\n";
          loop st
      | Ok ApplyAll ->
          print_endline "Applying all inference rules...";
          let st' = Sequent.apply_all_rules st in
          print_state st';
          loop st'
      | Ok (FindDerivations p) ->
          let derivations = Sequent.find_derivations st p in
          if derivations = [] then
            Printf.printf "No derivations found for: %s\n" (prop_to_string p)
          else (
            Printf.printf "Possible derivations for: %s\n" (prop_to_string p);
            List.iter
              (fun (rule_name, p1, p2) ->
                Printf.printf "  - %s: from %s and %s\n" rule_name
                  (prop_to_string p1) (prop_to_string p2))
              derivations);
          loop st
      | Ok ClearDerived ->
          let st' = Sequent.clear_derived st in
          print_endline "All derived formulas cleared.";
          print_state st';
          loop st'
      | Ok ClearGoal ->
          let st' = Sequent.clear_goal st in
          print_endline "Goal cleared.";
          print_state st';
          loop st'
      | Ok (RemovePremise p) ->
          let st' = Sequent.remove_premise st p in
          if st'.premises = st.premises then
            Printf.printf "Premise not found: %s\n" (prop_to_string p)
          else Printf.printf "Removed premise: %s\n" (prop_to_string p);
          print_state st';
          loop st'
      | Ok (Load filename) -> (
          if not (Sys.file_exists filename) then (
            Printf.printf "Error: file not found: %s\n" filename;
            loop st)
          else
            try
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
              loop st'
            with Sys_error msg ->
              Printf.printf "Error: could not read file %s: %s\n" filename msg;
              loop st))

(* -------------------------------------------------------------------------- *)
let () =
  print_banner ();
  print_help ();
  loop empty
