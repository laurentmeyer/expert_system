
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
 
let push_unique p q =
  if List.mem q p then q :: p else p

let rec search_query graph query =
  let rec search g p =
    print_endline ("\npile: " ^ string_of_pile p) ;
    print_endline ("graph:\n" ^ Graph.string_of_graph g) ;
    if p = [] then g
    else let conclusion = List.hd p in
      if Graph.not_in_graph g conclusion then search g (List.tl p) 
      else let conclusions = Graph.conclusions g conclusion in
        if List.filter (query_status g) conclusions != [] then search (Graph.add_truths g [conclusion]) (List.tl p) 
        else let to_push = conclusions
        |> List.filter ()
                           |> List.map (fun x -> [x ; Graph.not_of_ors x])
                           |> List.flatten
                           |> List.filter (fun x -> not (query_status g x))
                           |> List.filter (fun x -> List.mem x p)
          in
          (
            List.iter (fun x -> print_endline ("to push :\n" ^ Graph.string_of_or x)) to_push ;
            search g (to_push @ p)
          )
          (* 
          let update_with_condition g cond =
            let ands =
              Graph.split_ors_singleton cond
              |> List.filter (fun x -> not (query_status g x)) in
            if ands = [] then true
            else if List.filter (fun x -> List.mem x p) ands != [] then false
in
List.fold_left update 


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
end *)
  in
  let q = if query_status graph query then [] else [query; Graph.not_of_ors query] in
  search graph q

let search_all (system : System.system) =
  let searched_graph = List.fold_left search_query system.rules system.queries in
  let print_status q = Printf.printf "%S is %B\n" (Graph.string_of_or q) (query_status searched_graph q) in
  print_newline () ;
  List.iter print_status system.queries