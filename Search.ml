type t =
  {
    kb : Graph.graph ;
    inferred : (Graph.Facts.t * bool) list ;
    queue : Graph.Facts.t list ;
    path : Graph.Facts.t list ;
  }


(*  **************  SEARCH  ********************** *)



(*  **************  SERIALIZATION  *************** *)

let string_of_inferred (inferred : (Graph.Facts.t * bool) list) =
  let rec aux lst =
    match lst with
    | [] -> ""
    | (fact, b) :: [] -> Graph.string_of_fact fact ^ " " ^ (string_of_bool b)
    | (fact, b) :: tl -> Graph.string_of_fact fact ^ " " ^ (string_of_bool b) ^ "\n" ^ aux tl
  in aux inferred

let string_of_search (s : t) =
  let rec string_of_facts = function
  | [] -> ""
  | fact :: [] -> Graph.string_of_fact fact
  | fact :: tl -> Graph.string_of_fact fact ^ " |> " ^ string_of_facts tl
  in
  "** Knowledge base:\n" ^ Graph.string_of_graph s.kb ^ "\n" ^
  "** Inferred table:\n" ^ string_of_inferred s.inferred ^ "\n" ^
  "** Queue:\n" ^ string_of_facts s.queue ^ "\n" ^
  "** Path:\n" ^ string_of_facts s.path


(*  **************  INITIALIZATION  *************** *)

let list_facts (kb : Graph.graph) : Graph.Facts.t list =
  let get_facts (conc, cond) =
    conc :: Graph.not_of_fact conc :: (cond
             |> Graph.Ors.elements
             |> List.map Graph.Ands.elements
             |> List.flatten
             |> List.map (fun f -> [f ; Graph.not_of_fact f])
             |> List.flatten)
  in
  let rec aux acc lst = match lst with
    | head::tail  -> aux (get_facts head @ acc) tail 
    | []          -> acc
  in
  List.sort_uniq (Graph.Facts.compare) (aux [] kb)
  
let init_inferred kb =
  list_facts kb
  |> List.map (fun x -> (x, false))

let init_kb (system : System.system) : Graph.graph =
  Graph.add_truths system.rules system.truths

let init_search (system : System.system) : t =
  let knowledge_base = init_kb system
  in
  {
    kb = knowledge_base;
    inferred = init_inferred knowledge_base ;
    queue = [] ;
    path = [] ;
  }

let search_one (s : t) q : t =
  let new_s = { s with queue = [Graph.Ands.choose (Graph.Ors.choose q)]} in
  print_endline (string_of_search new_s) ;
  new_s


let search_all (system : System.system) : unit =
  let s = init_search system in
  let final_search = List.fold_left search_one s system.queries in
  ()