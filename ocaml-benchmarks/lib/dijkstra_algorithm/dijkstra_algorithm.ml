exception Invalid_input

(* Distance type. The distance is either Infinity or Some d for some
   floating-point number d. At the beginning of Dijkstra's algorithm, the
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

(* A pair of a vertex and a distance. This pair is stored inside each entry of a
   binary min-heap. In Dijkstra's algorithm, the distance refers to the shortest
   distance from the source to the current vertex. *)

type vertex_dist_pair = Vertex_dist of int * distance

let vertex_pair p =
  let _ = Raml.tick 1.0 in
  match p with Vertex_dist (v, _) -> v

let distance_pair p =
  let _ = Raml.tick 1.0 in
  match p with Vertex_dist (_, d) -> d

(* I use an imperative (i.e., array-based) implementation of a binary min-heap,
   instead of the functional (i.e., binary-tree-based) implementation used in
   heap sort and Huffman code.

   The benefit of the array-based implementation is that it supports the
   decrease-key operation, while the binary-tree-based implementation does not.
   In Dijkstra's algorithm, when we examine the neighbors of a vertex we just
   removed from the heap, we would like to update the priority (i.e., distance)
   of those neighbors that are in the heap. This is where the decrease-key
   operation comes in handy.

   In a functional implementation of Dijkstra's algorithm, we would need to
   create a new node and insert it to the heap, instead of updating an existing
   node in the heap. But in the imperative implementation, we can just update
   the priority of the node in the heap by the decrease-key operation. *)

(* The type binary_heap has three components: (i) the array storing all node in
   the heap, (ii) a natural number (of type Rnat.t) tracking the number of nodes
   in the heap, and (iii) a mapping from vertices (encoded by integers) to the
   array index in the heap that corresponds to this vertex. Thanks to this
   mapping, we can identify which node in the heap corresponds to a given vertex
   in constant time, which would be difficult in a functional implementation of
   Dijkstra's algorithm. *)

type binary_heap = vertex_dist_pair Rarray.t * Rnat.t * int Rarray.t

let rec heapify heap (index : int) =
  let _ = Raml.tick 1.0 in
  let left_index = (index * 2) + 1 and right_index = (index * 2) + 2 in
  let array, length, map_vertices_to_indices = heap in
  (* Figure out which of the current element (i.e., element at index) and the
     left child has a higher priority *)
  let smallest_index_left =
    if left_index < Rnat.to_int length then
      let element_index = Rarray.get array (Rnat.of_int index) in
      let element_left_index = Rarray.get array (Rnat.of_int left_index) in
      if
        is_shorter_distance
          (distance_pair element_left_index)
          (distance_pair element_index)
      then left_index
      else index
    else index
  in
  (* Figure out which of the element at the index smallest_index_left and the
     right child has a higher priority *)
  let smallest_index_right =
    if right_index < Rnat.to_int length then
      let element_smallest_index_left =
        Rarray.get array (Rnat.of_int smallest_index_left)
      in
      let element_right_index = Rarray.get array (Rnat.of_int right_index) in
      if
        is_shorter_distance
          (distance_pair element_right_index)
          (distance_pair element_smallest_index_left)
      then right_index
      else smallest_index_left
    else smallest_index_left
  in
  if smallest_index_right = index then ()
  else
    let element_at_index = Rarray.get array (Rnat.of_int index) in
    let (Vertex_dist (v_index, _)) = element_at_index in
    let element_at_smallest_index =
      Rarray.get array (Rnat.of_int smallest_index_right)
    in
    let (Vertex_dist (v_smallest_index, _)) = element_at_smallest_index in
    (* We swap the two elements: (i) the element at index and (ii) the element
       at the index smallest_index_right *)
    let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
    let _ =
      Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
    in
    (* We update the mapping from vertices to their corresponding array indices
    *)
    let _ =
      Rarray.set map_vertices_to_indices (Rnat.of_int v_index)
        smallest_index_right
    in
    let _ =
      Rarray.set map_vertices_to_indices (Rnat.of_int v_smallest_index) index
    in
    heapify heap smallest_index_right

let get_min heap =
  let _ = Raml.tick 1.0 in
  let array, _, _ = heap in
  Rarray.get array Rnat.zero

let delete_min heap =
  let _ = Raml.tick 1.0 in
  let array, length, map_vertices_to_indices = heap in
  Rnat.ifz length
    (fun () -> raise Invalid_input)
    (fun length_minus_one ->
      let first_element = Rarray.get array Rnat.zero in
      let (Vertex_dist (v_min, _)) = first_element in
      let last_element = Rarray.get array length_minus_one in
      let (Vertex_dist (v_last, _)) = last_element in
      (* We move the last element to the root of the heap *)
      let _ = Rarray.set array Rnat.zero last_element in
      (* We update the mapping from vertices to their array indices. Because the
         minimum element no longer exists in the heap, its array index is set to
         -1. *)
      let _ = Rarray.set map_vertices_to_indices (Rnat.of_int v_min) (-1) in
      let _ = Rarray.set map_vertices_to_indices (Rnat.of_int v_last) 0 in
      let _ = heapify heap 0 in
      (array, length_minus_one, map_vertices_to_indices))

let rec decrease_key_helper heap index =
  let _ = Raml.tick 1.0 in
  let array, _, map_vertices_to_indices = heap in
  if index = 0 then ()
  else
    let element_index = Rarray.get array (Rnat.of_int index) in
    let parent_index = (index - 1) / 2 in
    let parent_element = Rarray.get array (Rnat.of_int parent_index) in
    if
      is_shorter_distance
        (distance_pair element_index)
        (distance_pair parent_element)
    then
      let (Vertex_dist (v, _)) = element_index in
      let (Vertex_dist (v_parent, _)) = parent_element in
      (* We swap the current element and its parent *)
      let _ = Rarray.set array (Rnat.of_int index) parent_element in
      let _ = Rarray.set array (Rnat.of_int parent_index) element_index in
      (* We update the mapping from vertices to their array indices *)
      let _ = Rarray.set map_vertices_to_indices (Rnat.of_int v) parent_index in
      let _ = Rarray.set map_vertices_to_indices (Rnat.of_int v_parent) index in
      decrease_key_helper heap parent_index
    else ()

let decrease_key heap vertex new_dist =
  let _ = Raml.tick 1.0 in
  let array, _, map_vertices_to_indices = heap in
  let array_index = Rarray.get map_vertices_to_indices (Rnat.of_int vertex) in
  if array_index = -1 then ()
  else
    let (Vertex_dist (_, dist)) = Rarray.get array (Rnat.of_int array_index) in
    if is_shorter_distance new_dist dist then
      let _ =
        Rarray.set array (Rnat.of_int array_index)
          (Vertex_dist (vertex, new_dist))
      in
      decrease_key_helper heap array_index
    else ()

(* Initialize the array inside the heap by placing vertex v (encoded by an
   integer) at the array index v. Here, we assume that all vertices are
   identified by IDs from the interval [0, num_vertices), where num_vertices is
   the number of vertices in the graph. We also assume that the vertex 0 is the
   source in Dijkstra's algorithm. Hence, we set the distance of the vertex 0 to
   0 (because it is the source itself), and the distances of all other vertices
   to Infinity. *)

let rec initialize_array array index_nat =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index_nat
    (fun () -> ())
    (fun index_minus_one ->
      let _ =
        Rnat.ifz index_minus_one
          (fun () ->
            Rarray.set array index_minus_one
              (Vertex_dist (Rnat.to_int index_minus_one, Some 0.)))
          (fun _ ->
            Rarray.set array index_minus_one
              (Vertex_dist (Rnat.to_int index_minus_one, Infinity)))
      in
      initialize_array array index_minus_one)

(* Initialize the mapping from vertices to their array indices. Vertex v
   (encoded by an integer) is mapped to the index v. *)

let rec initialize_map_vertices_to_indices array index_nat =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index_nat
    (fun () -> ())
    (fun index_minus_one ->
      let _ = Rarray.set array index_minus_one (Rnat.to_int index_minus_one) in
      initialize_map_vertices_to_indices array index_minus_one)

(* Construct an initial heap from an adjacency list for an input graph. The
   adjacency list is implemented by an array of lists. *)

let construct_initial_heap adjacency_list =
  let _ = Raml.tick 1.0 in
  let num_vertices = Rarray.length adjacency_list in
  let array = Rarray.make num_vertices (Vertex_dist (-1, Infinity)) in
  let _ = initialize_array array num_vertices in
  let map_vertices_to_indices = Rarray.make num_vertices (-1) in
  let _ =
    initialize_map_vertices_to_indices map_vertices_to_indices num_vertices
  in
  (array, num_vertices, map_vertices_to_indices)

(* Given a vertex, extract a list of its neighbors from the adjacency list *)

let extract_neighbors adjacency_list (vertex : int) =
  let _ = Raml.tick 1.0 in
  let list_neighbors = Rarray.get adjacency_list (Rnat.of_int vertex) in
  list_neighbors

(* After we remove the vertex v from the heap, we examine all neighbors of the
   vertex v and update their distances in the heap. Here, the parameter
   base_dist refers to the shortest distance of the vertex v. *)

let rec update_dist_all_neighbors heap list_neighbors base_dist =
  let _ = Raml.tick 1.0 in
  match list_neighbors with
  | [] -> ()
  | (vertex, dist) :: tl ->
      let _ = decrease_key heap vertex (add_distances base_dist dist) in
      update_dist_all_neighbors heap tl base_dist

(* Repeatedly extract the minimum node from the heap *)

let rec repeatedly_get_min_node adjacency_list heap acc =
  let _ = Raml.tick 1.0 in
  let _, length, _ = heap in
  if Rnat.to_int length = 0 then acc
  else
    let min_node = get_min heap in
    let heap_updated = delete_min heap in
    let (Vertex_dist (vertex, dist)) = min_node in
    let list_neighbors = extract_neighbors adjacency_list vertex in
    let _ = update_dist_all_neighbors heap list_neighbors dist in
    let acc_updated = (vertex, dist) :: acc in
    repeatedly_get_min_node adjacency_list heap_updated acc_updated

(* Polynomial degree for AARA: N/A *)

let dijkstra_algorithm adjacency_list =
  let _ = Raml.tick 1.0 in
  let heap = construct_initial_heap adjacency_list in
  repeatedly_get_min_node adjacency_list heap []
