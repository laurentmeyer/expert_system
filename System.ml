type system =
  {
    rules : Graph.graph ;
    truths : Graph.Ors.t list ;
    queries : Graph.Facts.t list ;
  }

  let empty =
  {
    rules = [] ;
    truths = [] ;
    queries = [] ;
  }

(* DUMMY SYSTEM *)

(* let dummy_graph =
  [
    (
      Graph.Ors.(empty
            |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'A') |> add (Graph.Facts.Fact 'C'))
            |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'A') |> add (Graph.Facts.Fact 'D'))
            |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'B') |> add (Graph.Facts.Fact 'C'))
            |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'B') |> add (Graph.Facts.Fact 'D')))
      ,
      [
      ]
    )
    ;
    (
      Graph.Ors.(empty
            |> add (Graph.Ands.singleton (Graph.Facts.Fact 'E'))
            |> add (Graph.Ands.singleton (Graph.Facts.Fact 'F')))
      ,
      [
        Graph.Ors.(empty
              |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'A') |> add (Graph.Facts.Fact 'C'))
              |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'A') |> add (Graph.Facts.Fact 'D'))
              |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'B') |> add (Graph.Facts.Fact 'C'))
              |> add Graph.Ands.(empty |> add (Graph.Facts.Fact 'B') |> add (Graph.Facts.Fact 'D')))
      ]
    )
    ;
    (
      Graph.Ors.singleton (Graph.Ands.(empty |> add (Graph.Facts.Fact 'G') |> add (Graph.Facts.Fact 'H')))
      ,
      [
        Graph.Ors.(empty |> add (Graph.Ands.singleton (Graph.Facts.Fact 'E')) |> add (Graph.Ands.singleton (Graph.Facts.Fact 'F')))
      ]
    )
  ]

let dummy_truth = Graph.Ands.(empty |> add (Graph.Facts.Fact 'A') |> add (Graph.Facts.Fact 'D') |> add (Graph.Facts.Fact 'J'))
let dummy_query = [Graph.Facts.Fact 'G']  

let dummy_system =
  {
    rules = dummy_graph ; 
    truths = dummy_truth ;
    queries = dummy_query ;
  } *)

(* END OF DUMMY SYSTEM *)

let rec string_of_queries queries =
  match queries with
  | [] -> ""
  | fact :: [] -> Graph.string_of_fact fact
  | fact :: tl -> Graph.string_of_fact fact ^ ", " ^ string_of_queries tl

let rec string_of_truths truths =
  match truths with
  | [] -> ""
  | fact :: [] -> Graph.string_of_or fact
  | fact :: tl -> Graph.string_of_or fact ^ ", " ^ string_of_truths tl


let string_of_system (s : system) =
  "\n\n\n======EXPERT SYSTEM=====\n\ngraph:\n" ^ Graph.string_of_graph s.rules
  ^ "\ntruths: " ^ string_of_truths s.truths
  ^ "\n\nqueries: " ^ string_of_queries s.queries

let expand_system (s : system) =
  let expanded_graph = Graph.expand_graph (s.rules) in
  let graph_with_truths = Graph.add_truths expanded_graph s.truths in
  {s with rules = graph_with_truths}
