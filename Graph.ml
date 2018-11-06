module Facts =
struct
  type t = Fact of char | NotFact of char
  let compare t1 t2 =
    match (t1, t2) with
    | (Fact f1, Fact f2) -> Pervasives.compare f1 f2
    | (NotFact f1, NotFact f2) -> Pervasives.compare f1 f2
    | (Fact f1, NotFact f2) -> -1
    | (NotFact f1, Fact f2) -> 1
end

module Ands = Set.Make(Facts) (* était vertex*)
module Ors = Set.Make(Ands) (* était edge*)
type adjacency = Ors.t * Ors.t
type graph = adjacency list

exception Contradiction_exception of string

(*  **************  SERIALIZATION  *************** *)

let string_of_fact f =
  match f with
  | Facts.Fact f -> String.make 1 f
  | Facts.NotFact f -> "!" ^ String.make 1 f

let string_of_and a =
  let rec aux lst =
    match lst with
    | [] -> ""
    | fact :: [] -> string_of_fact fact
    | fact :: tl -> string_of_fact fact ^ "+" ^ aux tl
  in aux (Ands.elements a)

let string_of_or o =
  let rec aux lst =
    match lst with
    | [] -> ""
    | a :: [] -> string_of_and a
    | a :: tl -> string_of_and a ^ " | " ^ aux tl
  in aux (Ors.elements o)

let string_of_adjacency ((conclusion, condition) : adjacency) =
  string_of_or conclusion ^ " --> " ^ string_of_or condition ^ "\n"

let string_of_graph (g : graph) =
  let lst = List.map string_of_adjacency g in
  List.fold_left (^) "" lst


(*  **************  SEARCH  *************** *)

let vertices (graph : graph) : Ors.t list =
  List.map (fun (a, _) -> a) graph

let condition (graph : graph) vertex : Ors.t option =
  match List.find_opt (fun (a, _) -> a = vertex) graph with
  | Some (_, cond) -> Some cond
  | None -> None

let not_in_graph g o =
  not (List.mem o (vertices g))

let conclusions (graph : graph) vertex : Ors.t list =
  let unsorted = List.filter (fun x -> Ors.subset x vertex) (vertices graph) in
  let comp o1 o2 = Ors.cardinal o2 - Ors.cardinal o1 in
  List.sort comp unsorted

let split_ors_singleton o =
  Ors.choose o
  |> Ands.elements
  |> List.map Ands.singleton
  |> List.map Ors.singleton

(*  **************  UPDATES  *************** *)

let ors_of_char c : Ors.t =
  Ors.singleton (Ands.singleton (Fact c))

let union_ors a b : Ors.t =
  let remove_unnecessary ors =
    let rec aux queue acc = match queue with
      | [] -> Ors.of_list acc
      | hd :: tl when List.exists (fun x -> Ands.subset x hd) (acc @ tl) -> aux tl acc
      | hd :: tl -> aux tl (hd :: acc) in
    aux (Ors.elements ors) []
  in
  Ors.union a b
  |> remove_unnecessary 

let disunion_ors o : Ors.t list =
  let ors_list = Ors.elements o in
  List.map (Ors.singleton) ors_list

let intersection_ors or1 or2 =
  if or1 = or2 then or1 else
    let combine_or_and o a =
      Ors.elements o
      |> List.map (Ands.union a)
      |> List.map Ors.singleton
      |> List.fold_left union_ors Ors.empty
    in
    Ors.elements or1
    |> List.map (combine_or_and or2)
    |> List.fold_left union_ors Ors.empty

let not_of_ors ors =
  let not_of_fact fact = match fact with
    | Facts.Fact f -> Facts.NotFact f
    | Facts.NotFact f -> Facts.Fact f
  in
  let not_of_ands ands =
    Ands.elements ands
    |> List.map not_of_fact
    |> List.map Ands.singleton
    |> List.map Ors.singleton
    |> List.fold_left union_ors Ors.empty
  in
  let to_fold =
    Ors.elements ors
    |> List.map not_of_ands
  in
  List.fold_left intersection_ors (List.hd to_fold) to_fold

let xor_ors a b =
  union_ors (intersection_ors a (not_of_ors b)) (intersection_ors (not_of_ors a) b)

let add_adjacency graph (conclusion, condition) : graph =
  if condition = Ors.empty && List.mem (not_of_ors conclusion, Ors.empty) graph
  then raise (Contradiction_exception "Contradiction")
  else
    let is_conclusion (conc, _) = (conc = conclusion) in
    let (satisfies, not_satisfies) = List.partition is_conclusion graph in
    let new_adjacency = match satisfies with
      | [] -> (conclusion, condition)
      | _ when condition = Ors.empty -> (conclusion, condition)
      | (_, old_conditions) :: tl -> (conclusion, (union_ors old_conditions condition))
    in new_adjacency :: not_satisfies

let delete_adjacency graph conclusion : graph =
  let is_not_conclusion (conc, _) = (conc != conclusion) in
  List.filter is_not_conclusion graph

let remove_adjacency graph (conclusion, condition) : graph =
    let is_conclusion (conc, _) = (conc = conclusion) in
    let (satisfies, not_satisfies) = List.partition is_conclusion graph in
    let (_, old_conditions) = List.hd satisfies in
    let new_conditions = Ors.remove (Ors.choose condition) old_conditions in
    if Ors.is_empty new_conditions then not_satisfies
    else not_satisfies @ [(conclusion, new_conditions)]

let replace_adjacency graph (conclusion, condition) : graph =
  add_adjacency (delete_adjacency graph conclusion) (conclusion, condition)


let add_truths g t =
  t
  |> List.map (fun x -> (x, Ors.empty))
  |> List.fold_left add_adjacency g


(*  **************  EXPANSION  *************** *)

let add_contraposition g =
  let contrapose (conc, cond) = (not_of_ors cond, not_of_ors conc) in
  List.map contrapose g
  |> List.fold_left add_adjacency []
  |> List.append g

let expand_ors_in_conclusions g =
  let split_on_or (conc, cond) = 
    Ors.elements conc
    |> List.map (fun x -> (Ors.singleton x, Ors.remove x conc))
    |> List.map (fun (a,b) -> (a, not_of_ors b))
    |> List.map (fun (a,b) -> (a, intersection_ors cond b))
    |> List.cons (conc, cond)
  in
  let rec aux (queue : graph) acc =
    match queue with
    | [] -> acc
    | (conc, cond) :: tl when Ors.cardinal conc = 1 -> aux tl (acc @ [(conc, cond)])
    | hd :: tl -> aux tl (acc @ split_on_or hd) in
  aux g []
  |> List.fold_left add_adjacency []

let expand_ands_in_conclusions g =
  let split_on_ands (conc, cond) =
    Ors.choose conc
    |> Ands.elements
    |> List.map Ands.singleton
    |> List.map Ors.singleton
    |> List.map (fun x -> (x, cond))
  in
  let rec aux (queue : graph) acc =
    match queue with
    | [] -> acc
    | (conc, cond) :: tl when Ors.cardinal conc > 1 -> aux tl (acc @ [(conc, cond)])
    | (conc, cond) :: tl -> aux tl (acc @ split_on_ands (conc, cond))
  in
  aux g []
  |> List.fold_left add_adjacency []

let remove_inconsistencies g =
  let remove_ors_inconsistency (conc, cond) =
    let ands_has_inconsistency a =
      let rec aux lst =
        match lst with
        | [] -> false
        | Facts.Fact f :: tl -> List.mem (Facts.NotFact f) tl || aux tl
        | Facts.NotFact f :: tl -> List.mem (Facts.Fact f) tl || aux tl in
      aux (Ands.elements a) in
    let rec aux queue acc = match queue with
      | [] -> Ors.of_list acc
      | hd :: tl when ands_has_inconsistency hd -> aux tl acc
      | hd :: tl -> aux tl (hd :: acc) in
    (conc, aux (Ors.elements cond) [])
  in
  List.map remove_ors_inconsistency g
  |> List.filter (fun (a, b) -> Ors.is_empty b = false)

let deduct_truths g =
  let ors_had_truth ors =
    Ors.elements ors
    |> List.map (fun elt -> (elt, Ors.remove elt ors))
    |> List.map (fun (a, b) -> (not_of_ors (Ors.singleton a), b))
    |> List.exists (fun (a, b) -> Ors.subset a b)
  in
  g
  |> List.map (fun (a, b) -> if ors_had_truth b then (a, Ors.empty) else (a, b))
  |> List.fold_left add_adjacency []


let expand_graph graph =
  graph
  |> remove_inconsistencies (* a mettre directement dans l'ajout d'adjacency ?? *)
  |> add_contraposition
  (* |> deduct_truths *)
  |> expand_ors_in_conclusions
  |> expand_ands_in_conclusions
  |> deduct_truths (* a mettre directement dans l'ajout d'adjacency ?? *)