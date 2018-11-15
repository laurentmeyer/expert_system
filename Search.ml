type 'a tree = Leaf of 'a | NodeAnd of 'a tree list | NodeOr of 'a * 'a tree

(*  **************  SERIALIZATION  *************** *)

let rec depth t =
  match t with
  | Leaf _ -> 1
  | NodeAnd lst -> List.fold_left max 0 (List.map depth lst)
  | NodeOr (f, lst) -> 1 + depth lst

let string_of_tree t =
  let rec aux depth tree =
    let pad = String.make depth '|' in
    match tree with
    | NodeOr (f, a) -> pad ^ Graph.string_of_fact f ^ " is true because:\n" ^ aux (depth + 1) a
    | NodeAnd n     -> let cond_list = List.map (aux depth) n in List.fold_left (fun a b -> a ^ "\n" ^ pad ^ "and\n" ^ b) (List.hd cond_list) (List.tl cond_list)
    | Leaf f        -> pad ^ Graph.string_of_fact f ^ " is true"
  in
  (* make_colums (depth t) ^ "\n" ^ aux 0 t *)
  aux 0 t

(*  **************  SEARCH  ********************** *)

let unsome_list lst =
  let rec aux l acc =
    match l with
    | [] -> acc
    | Some s :: tl -> aux tl (s :: acc)
    | None :: tl -> aux tl acc
  in aux lst []

let rec or_search kb f path : Graph.Facts.t tree option =
  if List.mem f path then None else
  let path = f :: path in
  let (fact_is_positive, absolute_fact) = match f with
  | Graph.Facts.Fact _ -> (true, f)
  | Graph.Facts.NotFact g -> (false, Graph.Facts.Fact g) in
  match Graph.condition kb absolute_fact with
  | None when fact_is_positive -> None
  | None -> Some (Leaf f)
  | Some o when Graph.Ors.is_empty o && fact_is_positive -> Some (Leaf f)
  | Some o when Graph.Ors.is_empty o -> None
  | Some o -> begin
      let rec aux l =
        match l with
        | [] when fact_is_positive       -> None
        | []                             -> Some (Leaf f)
        | hd :: tl  -> match and_search kb hd path with
          | None                          -> aux tl
          | Some s when fact_is_positive  -> Some (NodeOr (f, s))
          | Some s                        -> None
      in aux (Graph.Ors.elements o)
    end
and and_search kb a path : Graph.Facts.t tree option =
  let results = List.map (fun f -> or_search kb f path) (Graph.Ands.elements a) in
  let failure = List.mem None results in
  if failure then None
  else Some (NodeAnd (unsome_list results))

let search (kb : Graph.graph) q : Graph.Facts.t tree option =
  or_search kb q []

(*  **************  INITIALIZATION  *************** *)

let search_all (system : System.system) : unit =
  let kb = Graph.add_truths system.rules system.truths in 
  let q = List.map (fun x -> x |> Graph.Ors.choose |> Graph.Ands.choose) system.queries in
  let results = List.map (fun x -> (x, search kb x)) q in
  let print_results r = 
  match r with
  | q, None      -> Printf.printf "%s is false\n\n" (Graph.string_of_fact q)
  (* | q, Some tree  -> Printf.printf "%s is true.\n" (Graph.string_of_fact q) *)
  | q, Some tree  -> Printf.printf "%s\n\n" (string_of_tree tree)
  in List.iter print_results results