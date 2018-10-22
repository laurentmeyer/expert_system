type fact = char

type truth =
  | Single of fact
  | Or of fact list
  (* | And of truth list *)

type implication =
  | True of truth
  | Not of truth
  | And of truth list
  (* | Or of implication list *)

type graph = (truth * implication list) list

let dummy_graph = [(Single 'A', [True (Single 'B')])]

let string_of_fact f =
  String.make 1 f

let string_of_truth f =
  let rec string_of_or lst =
    match lst with
    | [] -> ""
    | fact :: [] -> string_of_fact fact
    | fact :: tl -> string_of_fact fact ^ " | " ^ string_of_or tl
  in
  match f with
  | Single fact -> string_of_fact fact
  | Or l -> "(" ^ string_of_or l ^ ")"


let string_of_implications l =
  let string_of_implication i =
    match i with
    | True truth -> string_of_truth truth ^ " is true"
    | Not truth -> string_of_truth truth ^ " is not true"
    | And lst -> "*TODO* str of AND implication" in
  let rec aux lst =
    match lst with
    | [] -> ""
    | impl :: [] -> string_of_implication impl
    | impl :: tail -> string_of_implication impl ^ "\n" ^ aux tail
  in aux l

let string_of_rule (t, lst) =
  string_of_truth t ^ " is true if:\n" ^ string_of_implications lst

let string_of_graph g =
  let lst = List.map string_of_rule g in
  List.fold_left (^) "" lst