
(*  **************  SEARCH  *************** *)

let ors_is_true truths ors =
  let ands_is_true ands = Graph.Ands.subset ands truths in
  Graph.Ors.exists ands_is_true ors

let search_query graph truths query =
  let rec bfs queue marked =
    if queue = [] then marked
    else
      let v = List.hd queue in
      let adjacent = Graph.neighbors graph v in
      let is_unmarked i = not (List.mem i marked) in
      let unmarked = List.filter is_unmarked adjacent in
      bfs (List.tl queue @ unmarked) (marked @ unmarked) in
  let initial_query = Graph.Ors.singleton (Graph.Ands.singleton query) in
  let marked = bfs [initial_query] [initial_query] in
  let is_true = List.exists (ors_is_true truths) marked in
  Printf.printf "%s is %B\n" (Graph.string_of_fact query) is_true

let search_all (system : System.system) =
  List.iter (search_query system.rules system.truths) system.queries