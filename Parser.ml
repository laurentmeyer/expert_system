type operand = And | Or
type command = Truth | Query

type token =
  | LeftBracket
  | RightBracket
  | Operand of operand
  | Implication
  | Fact of char
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

let string_of_token t =
  match t with
  | LeftBracket -> "{ LEFT BRACKET }"
  | RightBracket -> "{ RIGHT BRACKET }"
  | Whitespace -> "{ WHITESPACE }"
  | Implication -> "{ IMPLICATION }"
  | Operand o -> "{ OPERAND: " ^ string_of_operand o ^ " }"
  | Fact f -> "{ FACT: " ^ Graph.string_of_fact (Fact f) ^ " }"
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
  | Operand _ -> "[|\\+]"
  | Implication -> "=>"
  | Fact _ -> "[A-Z]"
  | Command _ -> "[\\?=]"
  | Comment -> "#.*"
  | Whitespace -> "[ \\t]"

let regexp_of_token t = Str.regexp ("^" ^ string_of_token t)

let token_of_string str =
  if str = "(" then LeftBracket
  else if str = ")" then RightBracket
  else if str = "+" then Operand And
  else if str = "|" then Operand Or
  else if str = "=>" then Implication
  else if str = "=" then Command Truth
  else if str = "?" then Command Query
  else if Str.string_match (Str.regexp "^[ \\t]") str 0 then Whitespace
  else if Str.string_match (Str.regexp "^[##.*]") str 0 then Comment
  else if Str.string_match (Str.regexp "^[A-Z]$") str 0 then Fact (String.get str 0)
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

let all_tokens = [LeftBracket ; RightBracket ; Operand And ; Operand Or ; Implication ; Fact ' ' ; Comment ; Whitespace ; Command Truth ; Command Query]

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




let parse_file filename : System.system =
    let ic = open_in filename in
    let line = input_line ic in
    let p = lex_line line in
    print_endline (string_of_lexed p) ;
    close_in ic ;
    System.dummy_system