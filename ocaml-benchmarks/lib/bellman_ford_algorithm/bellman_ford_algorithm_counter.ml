exception Invalid_input

(* Counter-related functions. We have one counter for tracking the number of
   rounds (i.e., traversals over all edges in the graph) before the shortest
   distances are saturated (i.e., they remain unchanged). Concretely, the
   counter tracks the recursion depth of the function
   recursively_update_array_dist inside the function bellman_ford_algorithm. *)

let decrement_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  match current_counter with
  | [] -> raise Invalid_input
  | _ :: counter_tl -> (counter_tl, original_counter)

let increment_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  (1 :: current_counter, original_counter)

let initialize_counter current_original_counters =
  let _, original_counter = current_original_counters in
  (original_counter, original_counter)

let set_counter_to_zero current_original_counters =
  let _, original_counter = current_original_counters in
  ([], original_counter)

(* Bellman-Ford algorithm *)

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

let update_array_dist_single_vertex array_dist (vertex : int) new_dist =
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

let rec recursively_update_array_dist array_dist adjacency_list budget
    current_original_counters =
  let new_counter = decrement_counter current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
    let is_modified = update_array_dist_all_edges array_dist adjacency_list in
    if is_modified && budget > 1 then
      recursively_update_array_dist array_dist adjacency_list (budget - 1)
        new_counter
    else ((), new_counter)
  in
  (result, increment_counter counter_final)

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

(* Polynomial degree for AARA: 3 *)

let bellman_ford_algorithm adjacency_list current_original_counters =
  let _ = Raml.tick 1.0 in
  let array_dist = initialize_array_dist adjacency_list in
  let initialized_counter = initialize_counter current_original_counters in
  let _, counter1 =
    recursively_update_array_dist array_dist adjacency_list
      (Rnat.to_int (Rarray.length array_dist) - 1)
      initialized_counter
  in
  let counter2 = set_counter_to_zero counter1 in
  ( extract_output_from_array_dist array_dist (Rarray.length array_dist),
    counter2 )
