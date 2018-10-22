type token =
  | Unparsed
  | LeftParenthesis
  | RightParenthesis
  | Not
  | Operand
  | Implication
  | Value
  | Comment

type lexeme = token * string


let parse_file filename : System.system =
  try
    let ic = open_in filename in
    let line = input_line ic in
    (* print_endline line ; *)
    close_in ic ;
    System.dummy_system
  with
  | Failure err -> print_endline err ; System.dummy_system