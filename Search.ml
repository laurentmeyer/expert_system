
(*  **************  SERIALIZATION  *************** *)

let rec string_of_pile p =
  match p with
  | [] -> ""
  | fact :: [] -> Graph.string_of_or fact
  | fact :: tl -> Graph.string_of_or fact ^ " |> " ^ string_of_pile tl




(*  **************  SEARCH  *************** *)

let query_status g q =
  match Graph.condition g q with
  | Some o when o = Graph.Ors.empty -> true
  | _ -> false

let search_query graph query =
  let rec search g p =
    print_endline ("\npile: " ^ string_of_pile p) ;
    print_endline ("graph:\n" ^ Graph.string_of_graph g) ;
    match p with
    | [] -> g
    | hd :: tl when query_status g hd -> search g tl
    | hd :: tl -> begin
        g
      end
  in
  search graph [query; Graph.not_of_ors query]

let search_all (system : System.system) =
  let searched_graph = List.fold_left search_query system.rules system.queries in
  let print_status q = Printf.printf "%S is %B\n" (Graph.string_of_or q) (query_status searched_graph q) in
  print_newline () ;
  List.iter print_status system.queries