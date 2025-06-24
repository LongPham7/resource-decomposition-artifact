exception Invalid_input

type distance = Infinity | Some of float

let add_distances shortest_distance weight =
  let _ = Raml.tick 1.0 in
  match shortest_distance with
  | Infinity -> Infinity
  | Some d -> Some (d +. weight)

let is_shorter_distance distance1 distance2 =
  let _ = Raml.tick 1.0 in
  match (distance1, distance2) with
  | Some d1, Some d2 -> d1 < d2
  | Some _, Infinity -> true
  | _, _ -> false

let rec list_nat_length list =
  let _ = Raml.tick 1.0 in
  match list with [] -> Rnat.zero | _ :: tl -> Rnat.succ (list_nat_length tl)

let initialize_array_dist adjacency_list =
  let _ = Raml.tick 1.0 in
  let num_vertices = list_nat_length adjacency_list in
  let array_dist = Rarray.make num_vertices Infinity in
  let _ = Rarray.set array_dist Rnat.zero (Some 0.) in
  array_dist

let update_array_dist_single_vertex array_dist vertex new_dist =
  let _ = Raml.tick 1.0 in
  let old_dist = Rarray.get array_dist (Rnat.of_int vertex) in
  if is_shorter_distance new_dist old_dist then
    let _ = Rarray.set array_dist (Rnat.of_int vertex) new_dist in
    true
  else false

let rec update_array_dist_all_neighbors_helper array_dist shortest_dist
    list_neighbors =
  let _ = Raml.tick 1.0 in
  match list_neighbors with
  | [] -> false
  | (v, d) :: tl ->
      let new_dist = add_distances shortest_dist d in
      let is_modified1 =
        update_array_dist_single_vertex array_dist v new_dist
      in
      let is_modified2 =
        update_array_dist_all_neighbors_helper array_dist shortest_dist tl
      in
      is_modified1 || is_modified2

let update_array_dist_all_neighbors array_dist vertex list_neighbors =
  let _ = Raml.tick 1.0 in
  let shortest_dist = Rarray.get array_dist (Rnat.of_int vertex) in
  update_array_dist_all_neighbors_helper array_dist shortest_dist list_neighbors

let rec update_array_dist_all_edges array_dist adjacency_list =
  let _ = Raml.tick 1.0 in
  match adjacency_list with
  | [] -> false
  | (v, list_neighbors) :: tl ->
      let is_modified1 =
        update_array_dist_all_neighbors array_dist v list_neighbors
      in
      let is_modified2 = update_array_dist_all_edges array_dist tl in
      is_modified1 || is_modified2

let rec recursively_update_array_dist array_dist adjacency_list budget =
  let _ = Raml.tick 1.0 in
  let is_modified = update_array_dist_all_edges array_dist adjacency_list in
  if is_modified && budget > 1 then
    recursively_update_array_dist array_dist adjacency_list (budget - 1)
  else ()

let rec extract_output_from_array_dist array_dist index =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index
    (fun () -> [])
    (fun index_minus_one ->
      let recursive_result =
        extract_output_from_array_dist array_dist index_minus_one
      in
      let dist = Rarray.get array_dist index_minus_one in
      (Rnat.to_int index_minus_one, dist) :: recursive_result)

(* Miscellaneous functions for the purpose of running Hybrid AARA. Hybrid AARA
does not support the type Rnat. However, the implementation of the Bellman-Ford
algorithm uses the array type Rarray, which requires the type Rnat. Concretely,
we would like to enclose a function application of
recursively_update_array_dist with the annotation Raml.stat for data-driven
analysis because this is the smallest code fragment to which we need to apply
data-driven analysis.

To this end, we slightly modify the function recursively_update_array_dist so that its
type does not involve the array type Rarray. Specifically, we create a new
wrapper function recursively_update_array_dist_with_dist_array_initialization,
which does not take an array dist_array as the first input. It instead
internally creates the initial array dist_array without incurring computational
costs and then invokes the original function recursively_update_array_dist. *)

let initialize_array_dist_cost_free adjacency_list =
  let num_vertices = list_nat_length adjacency_list in
  let array_dist = Rarray.make num_vertices Infinity in
  let _ = Rarray.set array_dist Rnat.zero (Some 0.) in
  array_dist

let recursively_update_array_dist_with_dist_array_initialization adjacency_list budget =
  let array_dist = initialize_array_dist_cost_free adjacency_list in
  recursively_update_array_dist array_dist adjacency_list budget

let bellman_ford_algorithm adjacency_list =
  let _ = Raml.tick 1.0 in
  let array_dist = initialize_array_dist adjacency_list in
  let initial_budget = (Rnat.to_int (Rarray.length array_dist) - 1) in
  let _ =
    Raml.stat (recursively_update_array_dist_with_dist_array_initialization adjacency_list initial_budget)
  in
  extract_output_from_array_dist array_dist (Rarray.length array_dist)

let rec map f xs = 
  match xs with
  | [] -> []
  | hd :: tl -> (f hd) :: (map f tl)
