exception Invalid_input

(* An option type for an edge, which is represented by a pair (vertex, weight).
*)

type minimum_edge_option = None | Some of int * int

(* Initialize a list of vertex groups from the adjacency list of an input graph.
   Initially, each vertex forms its own vertex group.

   A vertex group consists of two components: (i) a list of vertices (from the
   original input graph) belonging to this vertex group and (ii) a list of edges
   incident on this vertex group. Throughout the execution of Boruvka's
   algorithm, we maintain the invariant that all edges connect two distinct
   vertex groups. Hence, whenever we fuse two vertex groups, we need to filter
   out internal edges (i.e., edges connecting two vertices belonging to the same
   vertex group) in order to restore the invariant. *)

let rec initialize_vertex_group adjacency_list =
  match adjacency_list with
  | [] -> []
  | (v, list_neighbors) :: tl ->
      ([ v ], list_neighbors) :: initialize_vertex_group tl

(* Identify the minimum edge of a given list of edges. If the input list of
   edges is empty, we return None. *)

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

(* For each vertex group in the graph, identify the minimum edge incident on it.
   For some vertex group, if the minimum edge cannot be identified, it means the
   list of edges incident on this vertex group is empty. That is, this vertex
   group is isolate from others. Therefore, we can safely drop the vertex group
   from the list of vertex groups. In fact, we must do so in order for AARA to
   infer a polynomial cost bound of Boruvka's algorithm.

   To prove termination of Boruvka's algorithm, AARA proves that the size of the
   list of vertex groups (i.e., the number of vertex groups plus the number of
   edges across all vertex groups) strictly decreases. For some vertex group, if
   no minimum edge is identified, the list of edges is not decreased at all by
   the function identify_minimum_edge. Therefore, we must decrease the number of
   vertex groups by dropping such a vertex group such that AARA can use the size
   of the list of vertex groups as a ranking function to prove termination (and
   to reason about complexity). *)

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

(* Check if a vertex exists in a given list of vertices *)

let rec find_vertex_in_list list_vertices (vertex : int) =
  match list_vertices with
  | [] -> false
  | hd :: tl -> if hd = vertex then true else find_vertex_in_list tl vertex

(* Given a list of vertex groups and a vertex v, extract the vertex group
   containing the vertex v. The function extract_vertex_group returns the vertex
   group and a list of remaining vertex groups. *)

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

(* Given a list of edges (i.e., pairs (vertex, weight)) and a list of vertices,
   remove any edge whose endpoint vertex is in the list of vertices. This
   function is used when we want to filter out internal edges when fusing two
   vertex groups. *)

let rec filter_out_internal_edges list_edges list_vertices =
  match list_edges with
  | [] -> []
  | (v, w) :: tl ->
      if find_vertex_in_list list_vertices v then
        filter_out_internal_edges tl list_vertices
      else (v, w) :: filter_out_internal_edges tl list_vertices

(* Update a list of vertex groups by fusing two vertex groups connected an input
   (minimum) edge. The function update_list_vertex_group_minimum_edge returns
   two outputs: (i) the updated list of vertex groups and (ii) an option value
   (i.e., None or Some edge) of the minimum edge. If the option value is None,
   it means that the minimum edge has already been added to the minimum spanning
   tree (MST) before. This happens when the same edge is selected twice in the
   same round of Boruvka's algorithm because this edge is the minimum edge for
   multiple vertex groups. Otherwise, Some edge means it is the first time to
   add this minimum edge to the MST. *)

let update_list_vertex_group_minimum_edge list_vertex_groups minimum_edge =
  let vertex1, vertex2, weight = minimum_edge in
  let vertex_group1, list_groups1 =
    extract_vertex_group list_vertex_groups vertex1
  in
  let list_vertices1, _ = vertex_group1 in
  if find_vertex_in_list list_vertices1 vertex2 then
    (* Interestingly enough, if we write list_vertex_groups below, in place of
        vertex_group1 :: list_groups1, AARA cannot infer a polynomial bound.

       To see why, note that the variable list_vertex_groups is mentioned in this
        branch of the if-else expression, but not in the else branch. Hence,
        when AARA parses the program, it notices that list_vertex_groups is used
        twice: the first time before the if-else expression and the second time
        inside the if-branch. As a result, AARA assigns potential to both
        occurrences of list_vertex_groups. However, the second occurrence of
        list_vertex_groups is only used inside the if-branch. In the else-branch,
        the potential assigned to list_vertex_groups will be wasted, causing
        AARA to fail to infer a polynomial cost bound. *)
    (vertex_group1 :: list_groups1, None)
  else
    (* Here, we perform pattern matching to define the variable list_edges1.
       Interestingly enough, if we do the pattern matching before the if-else
       expression (i.e., let (Vertex_group (list_vertices1, list_edges2)) =
       vertex_group1 in), then AARA will fail to infer a polynomial cost bound.
       This is probably because the variable list_edges1 is only used in the
       else-branch, but not in the if-branch. Hence, pattern matching on this
       variable before the if-else expression would lead to a waste potential in
       the if-branch, which does not use the variable. *)
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

(* Identify all minimum edges (incident on all vertex groups), and for each of
   them, we update the list of vertex groups. The function
   update_list_vertex_group returns two outputs: (i) the updated list of vertex
   groups and (ii) the set of unique minimum edges (i.e., all duplicates are
   removed). *)

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

(* Repeatedly identify minimum edges and update the list of vertex groups until
   we cannot identify minimum edges anymore (because only one vertex group
   remains). *)

let rec boruvka_algorithm_helper list_vertex_groups acc =
  match list_vertex_groups with
  | [] | [ _ ] -> acc
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

(* Polynomial degree for AARA: 4 *)

let boruvka_algorithm adjacency_list =
  let initial_list_vertex_groups = initialize_vertex_group adjacency_list in
  boruvka_algorithm_helper initial_list_vertex_groups []