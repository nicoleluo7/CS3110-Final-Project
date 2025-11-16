open Ast

(* Raised when the input string cannot be parsed as a valid formula *)
exception Parse_error of string

let trim = String.trim

(* Find the index where the "->" operator starts in string s. Returns: Some
   index if found, otherwise None. *)
let find_arrow s =
  let n = String.length s in
  let rec aux i =
    if i >= n - 1 then None
    else if s.[i] = '-' && s.[i + 1] = '>' then Some i
    else aux (i + 1)
  in
  aux 0

(* Find the index of '&' in s, if it exists. Returns: Some index if found,
   otherwise None. *)
let find_and s =
  let n = String.length s in
  let rec aux i =
    if i >= n then None else if s.[i] = '&' then Some i else aux (i + 1)
  in
  aux 0

(* Recursively parse a string into a propositional formula (type prop).
   Supported syntax: - Variables: A, B, C (must be a single uppercase letter) -
   Negation: !A - Conjunction: A & B - Implication: A -> B Parsing order (lowest
   to highest precedence): 1. Implication 2. Conjunction 3. Negation *)
let rec parse_prop s =
  let s = trim s in
  if s = "" then raise (Parse_error "Empty formula")
  else if s.[0] = '!' then
    (* Parse negation: !A *)
    let rest = String.sub s 1 (String.length s - 1) in
    Not (parse_prop rest)
  else
    (* Look for "->" first, since implication has lowest precedence *)
    match find_arrow s with
    | Some i ->
        let left = String.sub s 0 i in
        let right = String.sub s (i + 2) (String.length s - i - 2) in
        Imp (parse_prop left, parse_prop right)
    | None -> (
        (* Otherwise look for conjunction '&' *)
        match find_and s with
        | Some i ->
            let left = String.sub s 0 i in
            let right = String.sub s (i + 1) (String.length s - i - 1) in
            And (parse_prop left, parse_prop right)
        | None ->
            (* If we reach this case, the string should be a variable. For MS2,
               restrict variables to a single uppercase letter. *)
            let s = String.trim s in
            if String.length s = 1 then
              match s.[0] with
              | 'A' .. 'Z' -> Var s
              | _ -> raise (Parse_error ("Invalid variable: " ^ s))
            else raise (Parse_error ("Invalid variable: " ^ s)))
