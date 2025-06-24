exception Invalid_input

(* The type vertex_group has three components: (i) a list of the representative
   vertices of the currently existing vertex groups, (ii) an array of vertex
   groups, where each vertex groups stores a list of vertices belonging to this
   group and a list of edges connecting this group and another group, and (iii)
   a mapping (implemented by an array) from vertices to their representatives.
*)

type vertex_groups =
  int list * (int list * (int * int) list) Rarray.t * int Rarray.t

(* Initialize a graph *)

let rec initialize_list_active_vertex_groups nat_num_vertices =
  let _ = Raml.tick 1.0 in
  Rnat.ifz nat_num_vertices
    (fun () -> [])
    (fun num_vertices_minus_one ->
      let recursive_result =
        initialize_list_active_vertex_groups num_vertices_minus_one
      in
      Rnat.to_int num_vertices_minus_one :: recursive_result)

let rec initialize_array_vertex_groups array_vertex_groups adjacency_list =
  let _ = Raml.tick 1.0 in
  match adjacency_list with
  | [] -> ()
  | (v, list_neighbors) :: tl ->
      let _ =
        Rarray.set array_vertex_groups (Rnat.of_int v) ([ v ], list_neighbors)
      in
      initialize_array_vertex_groups array_vertex_groups tl

let rec initialize_map_representatives_helper map_representatives indices =
  let _ = Raml.tick 1.0 in
  Rnat.ifz indices
    (fun () -> ())
    (fun index_minus_one ->
      let _ =
        Rarray.set map_representatives index_minus_one
          (Rnat.to_int index_minus_one)
      in
      initialize_map_representatives_helper map_representatives index_minus_one)

let rec list_nat_length list =
  let _ = Raml.tick 1.0 in
  match list with [] -> Rnat.zero | _ :: tl -> Rnat.succ (list_nat_length tl)

let initialize_graph adjacency_list =
  let _ = Raml.tick 1.0 in
  let num_vertices = list_nat_length adjacency_list in
  let list_active_vertex_groups =
    initialize_list_active_vertex_groups num_vertices
  in
  let array_vertex_groups = Rarray.make num_vertices ([], []) in
  let _ = initialize_array_vertex_groups array_vertex_groups adjacency_list in
  let map_representatives = Rarray.make num_vertices 0 in
  let _ =
    initialize_map_representatives_helper map_representatives num_vertices
  in
  (list_active_vertex_groups, array_vertex_groups, map_representatives)

(* Identify the minimum edge *)

type minimum_edge_option = None | Some of int * int

let rec identify_minimum_edge (list_edges : (int * int) list) =
  let _ = Raml.tick 1.0 in
  match list_edges with
  | [] -> (None, [])
  | [ (v, w) ] -> (Some (v, w), [])
  | (v, w) :: tl -> (
      let minimum_edge_recursive, list_remaining_edges =
        identify_minimum_edge tl
      in
      match minimum_edge_recursive with
      | None -> (Some (v, w), list_remaining_edges)
      | Some (v2, w2) ->
          if w < w2 then (Some (v, w), (v2, w2) :: list_remaining_edges)
          else (Some (v2, w2), (v, w) :: list_remaining_edges))

let rec identify_minimum_edges_graph list_active_vertex_groups
    array_vertex_groups =
  let _ = Raml.tick 1.0 in
  match list_active_vertex_groups with
  | [] -> []
  | representative :: tl -> (
      let vertex_group, list_neighbors =
        Rarray.get array_vertex_groups (Rnat.of_int representative)
      in
      let minimum_edge, list_remaining_edges =
        identify_minimum_edge list_neighbors
      in
      match minimum_edge with
      | None -> identify_minimum_edges_graph tl array_vertex_groups
      | Some (v, w) ->
          let vertex_group_updated = (vertex_group, list_remaining_edges) in
          let _ =
            Rarray.set array_vertex_groups
              (Rnat.of_int representative)
              vertex_group_updated
          in
          let list_minimum_edges =
            identify_minimum_edges_graph tl array_vertex_groups
          in
          (representative, v, w) :: list_minimum_edges)

(* Fuse two vertex groups *)

let rec append xs ys =
  let _ = Raml.tick 1.0 in
  match xs with [] -> ys | hd :: tl -> hd :: append tl ys

let rec filter_out_internal_edges list_edges map_representatives
    (merged_representative : int) =
  let _ = Raml.tick 1.0 in
  match list_edges with
  | [] -> []
  | (v, w) :: tl ->
      let v_representative = Rarray.get map_representatives (Rnat.of_int v) in
      if v_representative = merged_representative then
        filter_out_internal_edges tl map_representatives merged_representative
      else
        (v, w)
        :: filter_out_internal_edges tl map_representatives
             merged_representative

let rec update_map_representatives map_representatives list_vertices
    new_representative =
  let _ = Raml.tick 1.0 in
  match list_vertices with
  | [] -> ()
  | hd :: tl ->
      let _ =
        Rarray.set map_representatives (Rnat.of_int hd) new_representative
      in
      update_map_representatives map_representatives tl new_representative

let update_list_vertex_groups_minimum_edge array_vertex_groups
    map_representatives minimum_edge =
  let _ = Raml.tick 1.0 in
  let vertex1, vertex2, weight = minimum_edge in
  let vertex1_representative =
    Rarray.get map_representatives (Rnat.of_int vertex1)
  in
  let vertex2_representative =
    Rarray.get map_representatives (Rnat.of_int vertex2)
  in
  if vertex1_representative = vertex2_representative then None
  else
    let vertex_group1, list_neighbors1 =
      Rarray.get array_vertex_groups (Rnat.of_int vertex1_representative)
    in
    let vertex_group2, list_neighbors2 =
      Rarray.get array_vertex_groups (Rnat.of_int vertex2_representative)
    in
    let vertex_group_merged = append vertex_group1 vertex_group2 in
    let list_neighbors1_filtered =
      filter_out_internal_edges list_neighbors1 map_representatives
        vertex2_representative
    in
    let list_neighbors2_filtered =
      filter_out_internal_edges list_neighbors2 map_representatives
        vertex1_representative
    in
    let list_neighbors_merged =
      append list_neighbors1_filtered list_neighbors2_filtered
    in
    let _ =
      update_map_representatives map_representatives vertex_group2
        vertex1_representative
    in
    let _ =
      Rarray.set array_vertex_groups
        (Rnat.of_int vertex1_representative)
        (vertex_group_merged, list_neighbors_merged)
    in
    Some (vertex2, weight)

let rec update_list_vertex_group array_vertex_groups map_representatives
    list_minimum_edges acc_unique_minimum_edges acc_active_vertex_groups =
  let _ = Raml.tick 1.0 in
  match list_minimum_edges with
  | [] -> (acc_unique_minimum_edges, acc_active_vertex_groups)
  | minimum_edge :: tl -> (
      let unique_minimum_edge =
        update_list_vertex_groups_minimum_edge array_vertex_groups
          map_representatives minimum_edge
      in
      match unique_minimum_edge with
      | None ->
          update_list_vertex_group array_vertex_groups map_representatives tl
            acc_unique_minimum_edges acc_active_vertex_groups
      | Some (v, w) ->
          let v_representative =
            Rarray.get map_representatives (Rnat.of_int v)
          in
          update_list_vertex_group array_vertex_groups map_representatives tl
            ((v, w) :: acc_unique_minimum_edges)
            (v_representative :: acc_active_vertex_groups))

(* Remove duplicates from a list of active vertex groups *)

let rec remove_duplicates_helper list_active_vertex_groups map_representatives
    array_booleans acc =
  let _ = Raml.tick 1.0 in
  match list_active_vertex_groups with
  | [] -> acc
  | v :: tl ->
      let v_representative = Rarray.get map_representatives (Rnat.of_int v) in
      let is_already_included =
        Rarray.get array_booleans (Rnat.of_int v_representative)
      in
      if is_already_included then
        remove_duplicates_helper tl map_representatives array_booleans acc
      else
        let _ = Rarray.set array_booleans (Rnat.of_int v_representative) true in
        remove_duplicates_helper tl map_representatives array_booleans (v :: acc)

let remove_duplicates_list_active_vertex_groups list_active_vertex_groups
    map_representatives original_num_vertices =
  let _ = Raml.tick 1.0 in
  let array_booleans = Rarray.make original_num_vertices false in
  remove_duplicates_helper list_active_vertex_groups map_representatives
    array_booleans []

(* Repeatedly identify the set of all minimum edges in the graph *)

let rec boruvka_algorithm_helper graph original_num_vertices acc =
  let _ = Raml.tick 1.0 in
  let list_active_vertex_groups, array_vertex_groups, map_representatives =
    graph
  in
  match list_active_vertex_groups with
  | [] | [ _ ] -> acc
  | _ ->
      let list_minimum_edges =
        identify_minimum_edges_graph list_active_vertex_groups
          array_vertex_groups
      in
      let list_minimum_edges_filtered, list_merged_active_vertex_groups =
        update_list_vertex_group array_vertex_groups map_representatives
          list_minimum_edges [] []
      in
      let acc_updated = append list_minimum_edges_filtered acc in
      let list_merged_vertex_groups_without_duplicates =
        remove_duplicates_list_active_vertex_groups
          list_merged_active_vertex_groups map_representatives
          original_num_vertices
      in
      let graph_updated =
        ( list_merged_vertex_groups_without_duplicates,
          array_vertex_groups,
          map_representatives )
      in
      boruvka_algorithm_helper graph_updated original_num_vertices acc_updated

(* Polynomial degree for AARA: N/A *)

let boruvka_algorithm adjacency_list =
  let _ = Raml.tick 1.0 in
  let original_num_vertices = list_nat_length adjacency_list in
  let initial_graph = initialize_graph adjacency_list in
  boruvka_algorithm_helper initial_graph original_num_vertices []
