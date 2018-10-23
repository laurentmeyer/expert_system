type system =
  {
    rules : Graph.graph ;
    truths : Graph.fact list ;
    queries : Graph.fact list ;
  }

let dummy_system =
  {
    rules = Graph.dummy_graph ; 
    truths = Graph.dummy_truth ;
    queries = Graph.dummy_query ;
  }

let string_of_system (s : system) =
  "======EXPERT SYSTEM=====\nrules:\n" ^ Graph.string_of_graph s.rules ^ "\n\ntruths: \n\nqueries: "