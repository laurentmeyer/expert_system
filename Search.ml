type t =
  {
    kb : Graph.graph ;
    (* inferred : (Graph.Facts.t * bool) list ; *)
    queue : Graph.Facts.t list ;
    path : Graph.Facts.t list ;
  }

type 'a tree = Leaf of 'a | NodeAnd of 'a tree list | NodeOr of 'a tree * ('a tree list)

let dummy_tree =
  NodeOr (
    Leaf (Graph.Facts.Fact 'E'),
    [
      NodeAnd
        [
          NodeOr (
            Leaf (Graph.Facts.Fact 'C'),
            [
              NodeAnd
                [
                  Leaf (Graph.Facts.Fact 'A')
                ]
              ;
              NodeAnd
                [
                  Leaf (Graph.Facts.Fact 'B')
                  ;
                  Leaf (Graph.Facts.Fact 'E')
                ]
            ]
          )
          ;
          Leaf (Graph.Facts.Fact 'D')
        ]
    ]
  )


(*  **************  SEARCH  ********************** *)

(* let search (s : System.system) : tree = *)


(*  **************  SERIALIZATION  *************** *)
let rec depth t =
  match t with
  | Leaf _ -> 1
  | NodeAnd lst -> 1 + List.fold_left max 0 (List.map depth lst)
  | NodeOr (f, lst) -> List.fold_left max 0 (List.map depth lst)

let make_colums d =
  let rec aux i acc =
    if i = 0 then acc
    else if i mod 2 = 0 then aux (i - 1) ("And\t" ^ acc)
    else aux (i - 1) ("Or\t" ^ acc)
  in
  aux d ""

let string_of_tree t =
  let rec aux depth tree =
    match tree with
    | Leaf f        -> String.make depth '\t' ^ Graph.string_of_fact f ^ "\n"
    | NodeAnd n     -> (List.map (aux depth) n |> List.fold_left Pervasives.(^) "") ^ "\n"
    | NodeOr (f, o) -> aux depth f ^ (List.map (aux (depth + 1)) o |> List.fold_left Pervasives.(^) "")
  in
  make_colums (depth t) ^ "\n" ^ aux 0 t

(* let string_of_inferred (inferred : (Graph.Facts.t * bool) list) =
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
   "** Path:\n" ^ string_of_facts s.path *)


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
    (* inferred = init_inferred knowledge_base ; *)
    queue = [] ;
    path = [] ;
  }

let search_one (s : t) q : t =
  (* rajouter la recherche de la contradiction ? *)
  let s = { s with queue = [Graph.Ands.choose (Graph.Ors.choose q)] } in
  (* print_endline (string_of_search s) ; *)

  { s with path = [] }


let search_all (system : System.system) : unit =
  print_endline (string_of_tree dummy_tree) ;
  let s = init_search system in
  let final_search = List.fold_left search_one s system.queries in
  ()