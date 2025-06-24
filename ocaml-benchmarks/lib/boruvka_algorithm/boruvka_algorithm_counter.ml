exception Invalid_input

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

type minimum_edge_option = None | Some of int * int

let rec initialize_vertex_group adjacency_list =
  match adjacency_list with
  | [] -> []
  | (v, list_neighbors) :: tl ->
      ([ v ], list_neighbors) :: initialize_vertex_group tl

let rec identify_minimum_edge (list_edges : (int * int) list) =
  match list_edges with
  | [] -> (None, [])
  | [ (v, w) ] -> (Some (v, w), [])
  | (v, w) :: tl -> (
      let _ = Raml.tick 1.0 in
      let minimum_edge_recursive, list_remaining_edges =
        identify_minimum_edge tl
      in
      match minimum_edge_recursive with
      | None -> (Some (v, w), list_remaining_edges)
      | Some (v2, w2) ->
          if w < w2 then (Some (v, w), (v2, w2) :: list_remaining_edges)
          else (Some (v2, w2), (v, w) :: list_remaining_edges))

let list_head input_list =
  match input_list with [] -> raise Invalid_input | hd :: _ -> hd

let rec identify_minimum_edges_graph list_vertex_groups =
  match list_vertex_groups with
  | [] -> ([], [])
  | (list_vertices, list_edges) :: tl -> (
      let minimum_edge, list_remaining_edges =
        identify_minimum_edge list_edges
      in
      match minimum_edge with
      | None -> identify_minimum_edges_graph tl
      | Some (v, w) ->
          let representative_vertex = list_head list_vertices in
          let vertex_group_updated = (list_vertices, list_remaining_edges) in
          let list_minimum_edges, list_vertex_groups_updated =
            identify_minimum_edges_graph tl
          in
          ( (representative_vertex, v, w) :: list_minimum_edges,
            vertex_group_updated :: list_vertex_groups_updated ))

let rec find_vertex_in_list list_vertices (vertex : int) =
  match list_vertices with
  | [] -> false
  | hd :: tl -> if hd = vertex then true else find_vertex_in_list tl vertex

let rec extract_vertex_group list_vertex_groups vertex =
  match list_vertex_groups with
  | [] -> raise Invalid_input
  | hd_vertex_group :: tl ->
      let list_vertices, list_edges = hd_vertex_group in
      if find_vertex_in_list list_vertices vertex then
        ((list_vertices, list_edges), tl)
      else
        let vertex_group, list_remaining_groups =
          extract_vertex_group tl vertex
        in
        (vertex_group, (list_vertices, list_edges) :: list_remaining_groups)

let rec append xs ys = match xs with [] -> ys | hd :: tl -> hd :: append tl ys

let rec filter_out_internal_edges list_edges list_vertices =
  match list_edges with
  | [] -> []
  | (v, w) :: tl ->
      if find_vertex_in_list list_vertices v then
        filter_out_internal_edges tl list_vertices
      else (v, w) :: filter_out_internal_edges tl list_vertices

let update_list_vertex_group_minimum_edge list_vertex_groups minimum_edge =
  let vertex1, vertex2, weight = minimum_edge in
  let vertex_group1, list_groups1 =
    extract_vertex_group list_vertex_groups vertex1
  in
  let list_vertices1, _ = vertex_group1 in
  if find_vertex_in_list list_vertices1 vertex2 then
    (vertex_group1 :: list_groups1, None)
  else
    let _, list_edges1 = vertex_group1 in
    let vertex_group2, list_groups2 =
      extract_vertex_group list_groups1 vertex2
    in
    let list_vertices2, list_edges2 = vertex_group2 in
    let list_vertices_merged = append list_vertices1 list_vertices2 in
    let list_filtered_edges1 =
      filter_out_internal_edges list_edges1 list_vertices2
    in
    let list_filtered_edges2 =
      filter_out_internal_edges list_edges2 list_vertices1
    in
    let list_edges_merged = append list_filtered_edges1 list_filtered_edges2 in
    ( (list_vertices_merged, list_edges_merged) :: list_groups2,
      Some (vertex2, weight) )

let rec update_list_vertex_group list_vertex_groups list_minimum_edges acc =
  match list_minimum_edges with
  | [] -> (list_vertex_groups, acc)
  | minimum_edge :: tl -> (
      let list_vertex_groups_updated, unique_minimum_edge =
        update_list_vertex_group_minimum_edge list_vertex_groups minimum_edge
      in
      match unique_minimum_edge with
      | None -> update_list_vertex_group list_vertex_groups_updated tl acc
      | Some (v, w) ->
          update_list_vertex_group list_vertex_groups_updated tl ((v, w) :: acc)
      )

let rec boruvka_algorithm_helper list_vertex_groups acc
    current_original_counters =
  let new_counter = decrement_counter current_original_counters in
  let result, counter_final =
    match list_vertex_groups with
    | [] | [ _ ] -> (acc, new_counter)
    | _ ->
        let list_minimum_edges, list_vertex_groups_without_minimum_edges =
          identify_minimum_edges_graph list_vertex_groups
        in
        let list_vertex_groups_updated, list_minimum_edges_filtered =
          update_list_vertex_group list_vertex_groups_without_minimum_edges
            list_minimum_edges []
        in
        let acc_updated = append list_minimum_edges_filtered acc in
        boruvka_algorithm_helper list_vertex_groups_updated acc_updated
          new_counter
  in
  (result, increment_counter counter_final)

(* Polynomial degree for AARA: 3 *)

let boruvka_algorithm adjacency_list current_original_counters =
  let initial_list_vertex_groups = initialize_vertex_group adjacency_list in
  let initialized_counter = initialize_counter current_original_counters in
  let result, counter_final =
    boruvka_algorithm_helper initial_list_vertex_groups [] initialized_counter
  in
  (result, set_counter_to_zero counter_final)
