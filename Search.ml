
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

let rec search_query graph query =
  let rec search g p =
    print_endline ("\npile: " ^ string_of_pile p) ;
    print_endline ("graph:\n" ^ Graph.string_of_graph g) ;
    if p = [] then g
    else
      let conclusion = List.hd p in
      match Graph.condition g conclusion with
      | None -> search g (List.tl p)
      | Some o when o = Graph.Ors.empty -> search g (List.tl p)
      | Some o -> begin
          let choose = Graph.Ors.choose o in
          let ands = choose
            |> Graph.Ands.elements
            |> List.map Graph.Ands.singleton
            |> List.map Graph.Ors.singleton in
          let new_g = List.fold_left search_query g ands in
          let are_all_true =
            List.map (query_status new_g) ands
            |> List.fold_left Pervasives.(&&) true
          in
          (* print_endline ("choose:\n" ^ Graph.string_of_or choose) ; *)
          print_endline ("new_graph:\n" ^ Graph.string_of_graph new_g) ;
          if are_all_true then search (Graph.add_truths new_g [List.hd p]) (List.tl p)
          else if Graph.Ors.cardinal o > 1 then
          let shrunk_graph = Graph.replace_adjacency g (conclusion, Graph.Ors.remove choose o) in
            search shrunk_graph p
          else search (Graph.delete_adjacency g conclusion) (List.tl p)
        end
  in
  let q = if query_status graph query then [] else [query; Graph.not_of_ors query] in
  search graph q

let search_all (system : System.system) =
  let searched_graph = List.fold_left search_query system.rules system.queries in
  let print_status q = Printf.printf "%S is %B\n" (Graph.string_of_or q) (query_status searched_graph q) in
  print_newline () ;
  List.iter print_status system.queries