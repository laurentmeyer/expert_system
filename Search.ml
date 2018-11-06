
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
      | Some o ->
        begin
          let first_ors = Graph.Ors.singleton (Graph.Ors.choose o) in
          let split = Graph.split_ors_singleton first_ors in
          match List.find_opt (fun x -> List.mem x p) split with
          | Some _ -> search (Graph.remove_adjacency g (conclusion, first_ors)) (List.tl p)
          | None ->
            begin
              let first_fact = List.hd split in
              match Graph.condition g first_fact with
              | None -> search (Graph.remove_adjacency g (conclusion, first_ors)) p
              | Some o when o != Graph.Ors.empty -> search g (first_fact :: p)
              | Some o -> begin
                  if List.tl split = [] then search (Graph.add_truths g [conclusion]) (List.tl p) 
                  else 
                    let new_condition = List.fold_left Graph.intersection_ors (List.hd (List.tl split)) (List.tl split) in
                    print_endline ("new comdition: " ^ Graph.string_of_or new_condition) ;
                    let new_graph = Graph.replace_adjacency g (conclusion, new_condition) in
                    search new_graph p
                end
            end
        end
  in
  let q = if query_status graph query then [] else [query; Graph.not_of_ors query] in
  search graph q

let search_all (system : System.system) =
  let searched_graph = List.fold_left search_query system.rules system.queries in
  let print_status q = Printf.printf "%S is %B\n" (Graph.string_of_or q) (query_status searched_graph q) in
  print_newline () ;
  List.iter print_status system.queries