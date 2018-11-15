type system =
  {
    rules : Graph.graph ;
    truths : Graph.Ors.t list ;
    queries : Graph.Ors.t list ;
  }

 let empty =
  {
    rules = [] ;
    truths = [] ;
    queries = [] ;
  }

let rec string_of_queries queries =
  match queries with
  | [] -> ""
  | fact :: [] -> Graph.string_of_or fact
  | fact :: tl -> Graph.string_of_or fact ^ ", " ^ string_of_queries tl

let rec string_of_truths truths =
  match truths with
  | [] -> ""
  | fact :: [] -> Graph.string_of_or fact
  | fact :: tl -> Graph.string_of_or fact ^ ", " ^ string_of_truths tl


let string_of_system (s : system) =
  "======EXPERT SYSTEM=====\n\ngraph:\n" ^ Graph.string_of_graph s.rules
  ^ "\ntruths: " ^ string_of_truths s.truths
  ^ "\n\nqueries: " ^ string_of_queries s.queries
  ^ "\n\n=========================\n"