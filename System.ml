type system =
  {
    rules : Graph.graph ;
    truths : Graph.Ands.t ;
    queries : Graph.Facts.t list ;
  }

let dummy_system =
  {
    rules = Graph.dummy_graph ; 
    truths = Graph.dummy_truth ;
    queries = Graph.dummy_query ;
  }

let rec string_of_queries queries =
  match queries with
  | [] -> ""
  | fact :: [] -> Graph.string_of_fact fact
  | fact :: tl -> Graph.string_of_fact fact ^ ", " ^ string_of_queries tl


let string_of_system (s : system) =
  "\n\n\n======EXPERT SYSTEM=====\n\ngraph:\n" ^ Graph.string_of_graph s.rules
  ^ "\ntruths: " ^ Graph.string_of_and s.truths
  ^ "\n\nqueries: " ^ string_of_queries s.queries