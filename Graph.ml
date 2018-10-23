type fact = Fact of char | NotFact of char
type vertex = fact list
type edge = vertex list
type adjacency = vertex * edge list
type graph = adjacency list

let dummy_truth = [Fact 'C']
let dummy_query = [Fact 'A']
let dummy_graph : graph =
  let dummy_fact : fact = Fact 'D' in
  let dummy_vertex : vertex = [dummy_fact] in
  let dummy_edge : edge = [[Fact 'A'] ; [Fact 'B' ; Fact 'C']] in
  let dummy_adjacency : adjacency = (dummy_vertex, [dummy_edge]) in
  [dummy_adjacency]



(*  **************  SERIALIZATION  *************** *)

let string_of_fact f =
  match f with
  | Fact f -> String.make 1 f
  | NotFact f -> "!" ^ String.make 1 f

let rec string_of_vertex (vertex : vertex) =
  match vertex with
  | [] -> ""
  | fact :: [] -> string_of_fact fact
  | fact :: tl -> string_of_fact fact ^ " | " ^ string_of_vertex tl

let rec string_of_edge (edge : edge) =
  match edge with
  | [] -> ""
  | vertex :: [] -> "(" ^ string_of_vertex vertex ^ ")"
  | vertex :: tl -> "(" ^ string_of_vertex vertex ^ ")" ^ " + " ^ string_of_edge tl

let string_of_adjacency ((vertex, edges) : adjacency) =
  let string_of_pair e = string_of_edge e ^ " => " ^ string_of_vertex vertex ^ "\n" in
  let to_fold = List.map string_of_pair edges in
  List.fold_left (^) "" to_fold 

let string_of_graph (g : graph) =
  let lst = List.map string_of_adjacency g in
  List.fold_left (^) "" lst


(*  **************  UPDATES  *************** *)


(* let create_edge graph vertex edge =



   let rec update_graph graph vertex edge : graph =
    let (t, tail) = List.partition (fun (a, b) -> a = truth) graph in
    let new_truth = match t with
      | [] -> (vertex, [edge]) :: graph
      | (_, i) :: _ -> (vertex, edge :: i) :: graph
    in new_truth :: tail

   and create_

   and update_truth graph vertex edge =
    match vertex with
    | Fact _ -> (vertex, []) :: graph
    | Or lst ->
      begin
        let fact_list : edge list = lst in
        (vertex, lst) :: graph
      end

   (* and create_or graph vertex (lst : fact list) : graph =
   let implication_list = List.map (fun f -> Fa)
   List.fold_left create_truth_if_necessary  *)




   let add_implication (graph : graph) vertex edge : graph =
   let graph = add_truth_if_necessary graph vertex in

   let filter_fun = fun (a, b) -> a = vertex in
   let (is_truth, other) = List.partition filter_fun graph in
   if is_truth = [] then graph @ [(vertex, [edge])]
   else
    let (_, i) = List.hd is_truth in
    other @ [(vertex, i @ [edge])] *)