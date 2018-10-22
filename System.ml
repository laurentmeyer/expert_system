type system =
  {
    rules : Graph.graph ;
    truths : Graph.truth ;
    queries : Graph.truth ;
  }

let dummy_system =
  {
    rules = Graph.dummy_graph ; 
    truths = Graph.Single 'A' ;
    queries = Graph.Single 'B' ;
  }

let string_of_system (s : system) =
  "======EXPERT SYSTEM=====\nrules:\n" ^ Graph.string_of_graph s.rules ^ "\n\ntruths: \n\nqueries: "