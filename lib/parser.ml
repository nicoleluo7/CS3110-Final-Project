open Ast

exception Parse_error of string
(** Raised when the input string cannot be parsed as a valid formula *)

let trim = String.trim

let rec find_space s i =
  if i >= String.length s then None
  else if s.[i] = ' ' then Some i
  else find_space s (i + 1)

(** Find the position of the closing parenthesis that matches the opening one at
    index 0. Returns None if parentheses are unbalanced. *)
let find_matching_paren s =
  let len = String.length s in
  if len = 0 || s.[0] <> '(' then None
  else
    let rec aux i depth =
      if i >= len then None
      else
        match s.[i] with
        | '(' -> aux (i + 1) (depth + 1)
        | ')' -> if depth = 1 then Some i else aux (i + 1) (depth - 1)
        | _ -> aux (i + 1) depth
    in
    aux 1 1

(** Remove a single pair of surrounding parentheses if they wrap the entire
    string. Repeats until no more wrapping parentheses remain. *)
let rec strip_outer_parens s =
  let s = trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' then
    match find_matching_paren s with
    | Some idx when idx = len - 1 ->
        let inner = String.sub s 1 (len - 2) in
        strip_outer_parens inner
    | Some _ -> s
    | None -> raise (Parse_error "Unmatched '('")
  else s

(** Helper to locate the first top-level operator that satisfies the predicate,
    ignoring any operators that occur inside parentheses. *)
let find_top_level predicate s =
  let len = String.length s in
  let depth = ref 0 in
  let found = ref None in
  for i = 0 to len - 1 do
    match s.[i] with
    | '(' -> depth := !depth + 1
    | ')' ->
        if !depth = 0 then raise (Parse_error "Unmatched ')'")
        else depth := !depth - 1
    | _ -> if !depth = 0 && !found = None && predicate i then found := Some i
  done;
  if !depth <> 0 then raise (Parse_error "Unmatched '('");
  !found

let find_arrow s =
  find_top_level
    (fun i -> i < String.length s - 1 && s.[i] = '-' && s.[i + 1] = '>')
    s

let find_and s = find_top_level (fun i -> s.[i] = '&') s

let find_or s = find_top_level (fun i -> s.[i] = '|') s

let is_var_char = function
  | 'A' .. 'Z' -> true
  | _ -> false

(** Recursively parse a string into its simplified propositional formula (type
    prop).*)
let rec parse_prop s =
  let rec parse_raw s =
    let s = trim s in
    if s = "" then raise (Parse_error "Empty formula")
    else if s.[0] = '!' then
      let rest = String.sub s 1 (String.length s - 1) in
      Not (parse_raw rest)
    else
      let s = strip_outer_parens s in
      match find_arrow s with
      | Some idx ->
          let left = String.sub s 0 idx in
          let right = String.sub s (idx + 2) (String.length s - idx - 2) in
          Imp (parse_raw left, parse_raw right)
      | None -> (
          match find_and s with
          | Some idx ->
              let left = String.sub s 0 idx in
              let right = String.sub s (idx + 1) (String.length s - idx - 1) in
              And (parse_raw left, parse_raw right)
          | None -> (
              match find_or s with
              | Some idx ->
                  let left = String.sub s 0 idx in
                  let right =
                    String.sub s (idx + 1) (String.length s - idx - 1)
                  in
                  Or (parse_raw left, parse_raw right)
              | None ->
                  let s = trim s in
                  if String.length s = 1 && is_var_char s.[0] then Var s
                  else raise (Parse_error ("Invalid variable: " ^ s))
            )
        )
  in
  simplify (parse_raw s)
