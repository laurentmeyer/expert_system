type 'a tree = Leaf of 'a | NodeAnd of 'a tree list | NodeOr of 'a tree * ('a tree list)

(*  **************  SERIALIZATION  *************** *)

let rec depth t =
  match t with
  | Leaf _ -> 1
  | NodeAnd lst -> List.fold_left max 0 (List.map depth lst)
  | NodeOr (f, lst) -> 1 + List.fold_left max 0 (List.map depth lst)

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

(*  **************  SEARCH  ********************** *)

let unsome_list lst =
  let rec aux l acc =
    match l with
    | [] -> acc
    | Some s :: tl -> aux tl (s :: acc)
    | None :: tl -> aux tl acc
  in aux lst []

let rec is_in_tree fact tree : bool = 
  let is_in_any lst = List.map (is_in_tree fact) lst
                      |> List.fold_left Pervasives.(||) false
                      |> not in
  match tree with
  | Leaf f -> f = fact
  | NodeAnd lst -> is_in_any lst
  | NodeOr (_, lst) -> is_in_any lst

let rec or_search kb f : Graph.Facts.t tree option =
  (* verifier que pas deja dans le tree *)
  match Graph.condition kb f with
  | None -> None
  | Some o when Graph.Ors.is_empty o -> Some (Leaf f)
  | Some o -> begin
      let rec aux l =
        match l with
        | [] -> None
        | hd :: tl -> match and_search kb hd with
          | None                        -> aux tl
          (* | Some s when is_in_tree f s  -> aux tl *)
          | Some s                      -> Some (NodeOr (Leaf f, [s]))
      in aux (Graph.Ors.elements o)
    end
and and_search kb a : Graph.Facts.t tree option =
  let results = List.map (fun f -> or_search kb f) (Graph.Ands.elements a) in
  let failure = List.mem None results in
  if failure then None
  else Some (NodeAnd (unsome_list results))

let search (kb : Graph.graph) q : Graph.Facts.t tree option =
  or_search kb q

(*  **************  INITIALIZATION  *************** *)

let search_all (system : System.system) : unit =
  let kb = Graph.add_truths system.rules system.truths in 
  let result = search kb (List.hd system.queries |> Graph.Ors.choose |> Graph.Ands.choose) in
  match result with
  | None -> print_endline "echec"
  | Some tree -> print_endline (string_of_tree tree)
