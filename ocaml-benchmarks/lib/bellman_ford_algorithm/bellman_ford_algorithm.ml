exception Invalid_input

(* Distance type. The distance is either Infinity or Some d for some
   floating-point number d. At the beginning of the Bellman-Ford algorithm, the
   shortest distances of all vertices, except for the source, are initialized to
   Infinity. *)

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

(* Compute the length of an input list and return it as a natural number *)

let rec list_nat_length list =
  let _ = Raml.tick 1.0 in
  match list with [] -> Rnat.zero | _ :: tl -> Rnat.succ (list_nat_length tl)

(* Initialize an array that maps vertices to their (candidate) shortest distances
   from the source vertex. The source vertex, which is assumed to be the first
   element in the adjacency list, has the initialized shortest distance of zero.
   All other vertices have the initialized shortest distance of Infinity. *)

let initialize_array_dist adjacency_list =
  let _ = Raml.tick 1.0 in
  let num_vertices = list_nat_length adjacency_list in
  let array_dist = Rarray.make num_vertices Infinity in
  let _ = Rarray.set array_dist Rnat.zero (Some 0.) in
  array_dist

(* Update the shortest distance of a single entry in the array array_dist,
   which maps vertices to their (candidate) shortest distances. The function
   update_array_dist_single_vertex returns whether the list list_dist was
   modified. We need this information because the saturation-based Bellman-Ford
   algorithm repeatedly updates array_dist until it is saturated (i.e., it
   cannot be updated anymore). *)

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

(* We go over all edges in the adjacency list and update the candidate shortest
   distances stored in the array array_dist. Here, the adjacency list is encoded
   as a list of lists.contents I didn't use an array of lists because to encode
   the adjacency list, because we do not need constant-time access to a vertex's
   list of neighbors - we traverse over all edges anyway. *)

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

(* Extract a list of shortest distances of all vertices [0, index) in the array
   array_dist. When we first call this function, we should set the input index
   to the length of array_dist (i.e., number of vertices in the graph). *)

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

(* Polynomial degree for AARA: N/A *)

let bellman_ford_algorithm adjacency_list =
  let _ = Raml.tick 1.0 in
  let array_dist = initialize_array_dist adjacency_list in
  let _ =
    recursively_update_array_dist array_dist adjacency_list
      (Rnat.to_int (Rarray.length array_dist) - 1)
  in
  extract_output_from_array_dist array_dist (Rarray.length array_dist)
