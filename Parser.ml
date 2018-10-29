type operand = And | Or | Xor
type command = Truth | Query

type token =
  | LeftBracket
  | RightBracket
  | Not
  | Operand of operand
  | Implication
  | DoubleImplication
  | Fact of Graph.Ors.t
  | Command of command
  | Comment
  | Whitespace

exception Parsing_exception of string

(*  **************  SERIALIZATION  *************** *)

let string_of_command c =
  match c with
  | Query -> "?"
  | Truth -> "="

let string_of_operand o =
  match o with
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"

let string_of_token t =
  match t with
  | LeftBracket -> "{ LEFT BRACKET }"
  | RightBracket -> "{ RIGHT BRACKET }"
  | Not -> "{ NOT }"
  | Whitespace -> "{ WHITESPACE }"
  | Implication -> "{ IMPLICATION }"
  | DoubleImplication -> "{ DOUBLE IMPLICATION }"
  | Operand o -> "{ OPERAND: " ^ string_of_operand o ^ " }"
  | Fact f -> "{ FACT: " ^ Graph.string_of_or f ^ " }"
  | Comment -> "{ COMMENT }"
  | Command c -> "{ COMMAND: " ^ string_of_command c ^ " }"


let rec string_of_lexed p =
  match p with
  | [] -> ""
  | token :: [] -> string_of_token token
  | token :: tail -> string_of_token token ^ "\n" ^ string_of_lexed tail

(*  **************  LEXING  *************** *)

let string_of_token t =
  match t with
  | LeftBracket -> "("
  | RightBracket -> ")"
  | Not -> "!"
  | Operand _ -> "[|\\+\\^]"
  | Implication -> "=>"
  | DoubleImplication -> "<=>"
  | Fact _ -> "[A-Z]"
  | Command _ -> "[\\?=]"
  | Comment -> "#.*"
  | Whitespace -> "[ \\t]"

let regexp_of_token t = Str.regexp ("^" ^ string_of_token t)

let token_of_string str =
  if str = "(" then LeftBracket
  else if str = ")" then RightBracket
  else if str = "!" then Not
  else if str = "+" then Operand And
  else if str = "|" then Operand Or
  else if str = "^" then Operand Xor
  else if str = "=>" then Implication
  else if str = "<=>" then DoubleImplication
  else if str = "=" then Command Truth
  else if str = "?" then Command Query
  else if Str.string_match (Str.regexp "^[ \\t]") str 0 then Whitespace
  else if Str.string_match (Str.regexp "^[##.*]") str 0 then Comment
  else if Str.string_match (Str.regexp "^[A-Z]$") str 0 then Fact (Graph.ors_of_char (String.get str 0))
  else raise (Parsing_exception str)

let cascade_lexing res token =
  match res with
  | Ok l -> Ok l
  | Error (str, token_list) -> begin
      let regexp = regexp_of_token token in
      let did_match = Str.string_match regexp str 0 in
      if not did_match then Error (str, token_list)
      else
        let matched_str = Str.matched_string str in
        let new_token_list = token_list @ [token_of_string matched_str] in
        let new_str = Str.string_after str (String.length matched_str) in
        Ok (new_str, new_token_list)
    end

let all_tokens = [LeftBracket ; RightBracket; Not ; Operand And ; Operand Or ; Operand Xor ; Implication ; DoubleImplication ; Fact Graph.Ors.empty ; Comment ; Whitespace ; Command Truth ; Command Query]

let lex_line line =
  let rec aux lexing =
    let (str, token_list) = lexing in
    if str = "" then token_list
    else
      match List.fold_left cascade_lexing (Error lexing) all_tokens with
      | Ok l -> aux l
      | Error _ -> raise (Parsing_exception str)
  in
  let unfiltered_token = aux (line, []) in
  let filter_token t =
    match t with
    | Comment -> false
    | Whitespace -> false
    | _ -> true in
  List.filter filter_token unfiltered_token

(*  **************  PARSING  *************** *)
(* | LeftBracket       -> LeftBracket, Not, Fact
| RightBracket      -> Operand, Implication, DoubleImplication
| Not               -> LeftBracket, Fact
| Operand           -> No, Fact, LeftBracket 
| Implication       -> LeftBracket, Not, Fact
| DoubleImplication -> LeftBracket, Not, Fact
| Fact              -> Operand, RightBracket, Implication, DoubleImplication

| Command
| Comment
| Whitespace *)


let rec parse_expression tokens =
  match tokens with
  | Fact f :: [] -> f
  | Fact a :: Operand Or :: Fact b :: tail -> parse_expression (Fact (Graph.union_ors a b) :: tail)
  | _ -> raise (Parsing_exception "cas a gerer")


let split_rule tokens =
  let rec aux acc rhs =
    match rhs with
    | Implication :: tail -> (acc, tail)
    | DoubleImplication :: tail -> (acc, tail)
    | hd :: tail -> aux (acc @ [hd]) tail
    | _ -> raise (Parsing_exception "Parser: no `=>' or `<=>' in the rule.") in
  aux [] tokens

let parse_rule (tokens : token list) (system : System.system) : System.system =
  let (left_side, right_side) = split_rule tokens in
  (* print_endline (string_of_lexed left_side) ;
  print_endline (string_of_lexed right_side) ; *)
  let new_left = parse_expression left_side in
  let new_right = parse_expression right_side in
  let new_graph = Graph.add_adjacency system.rules new_left new_right in
  (* print_endline (string_of_lexed new_left) ;
  print_endline (string_of_lexed new_right) ; *)
  {system with rules = new_graph }

let parse_tokens (tokens : token list) system : System.system =
  match tokens with
  | [] -> system
  | Command c :: _ -> system
  | _ -> parse_rule tokens system

let parse_file filename : System.system =
  let ic = open_in filename in
  let line = input_line ic in
  let tokens = lex_line line in
  let system = System.empty in
  let new_system = parse_tokens tokens system in
  print_endline (System.string_of_system new_system);
  close_in ic;
  new_system