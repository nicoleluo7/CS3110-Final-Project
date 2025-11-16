open Ast
open Parser

(* Simple REPL (read–eval–print loop) for testing the parser. This repeatedly
   asks the user for a formula, parses it, prints the result, and continues
   until the user types "quit". *)
let rec loop () =
  print_endline "Enter a propositional formula (or 'quit'):";
  match read_line () with
  | exception End_of_file -> () (* Exit cleanly on Ctrl+D *)
  | line ->
      let trimmed = String.trim line in
      if trimmed = "quit" then () (* Stop the loop and exit *)
      else begin
        (* Try to parse the user’s input and show the result *)
        (try
           let f = parse_prop line in
           print_endline ("Parsed as: " ^ to_string f)
         with Parse_error msg -> print_endline ("Parse error: " ^ msg));
        (* Ask for another input *)
        loop ()
      end

(* Start the REPL *)
let () = loop ()
