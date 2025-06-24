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

type vertex_dist_pair = Vertex_dist of int * distance

let vertex_pair p =
  let _ = Raml.tick 1.0 in
  match p with Vertex_dist (v, _) -> v

let distance_pair p =
  let _ = Raml.tick 1.0 in
  match p with Vertex_dist (_, d) -> d

type binary_heap = vertex_dist_pair Rarray.t * Rnat.t * int Rarray.t

let rec heapify heap (index : int) =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    let left_index = (index * 2) + 1 and right_index = (index * 2) + 2 in
    let array, length, map_vertices_to_indices = heap in
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
      let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
      let _ =
        Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
      in
      let _ =
        Rarray.set map_vertices_to_indices (Rnat.of_int v_index)
          smallest_index_right
      in
      let _ =
        Rarray.set map_vertices_to_indices (Rnat.of_int v_smallest_index) index
      in
      heapify heap smallest_index_right
  in
  let _ = Raml.mark 0 (-1.0) in
  result

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
      let _ = Rarray.set array Rnat.zero last_element in
      let _ = Rarray.set map_vertices_to_indices (Rnat.of_int v_min) (-1) in
      let _ = Rarray.set map_vertices_to_indices (Rnat.of_int v_last) 0 in
      let _ = Raml.activate_counter_variable 0 in
      let _ = heapify heap 0 in
      let _ = Raml.record_counter_variable 0 in
      (array, length_minus_one, map_vertices_to_indices))

let rec decrease_key_helper heap index =
  let _ = Raml.mark 1 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
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
        let _ = Rarray.set array (Rnat.of_int index) parent_element in
        let _ = Rarray.set array (Rnat.of_int parent_index) element_index in
        let _ =
          Rarray.set map_vertices_to_indices (Rnat.of_int v) parent_index
        in
        let _ =
          Rarray.set map_vertices_to_indices (Rnat.of_int v_parent) index
        in
        decrease_key_helper heap parent_index
      else ()
  in
  let _ = Raml.mark 1 (-1.0) in
  result

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
      let _ = Raml.activate_counter_variable 1 in
      let result = decrease_key_helper heap array_index in
      let _ = Raml.record_counter_variable 1 in
      result
    else ()

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

let rec initialize_map_vertices_to_indices array index_nat =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index_nat
    (fun () -> ())
    (fun index_minus_one ->
      let _ = Rarray.set array index_minus_one (Rnat.to_int index_minus_one) in
      initialize_map_vertices_to_indices array index_minus_one)

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

let extract_neighbors adjacency_list (vertex : int) =
  let _ = Raml.tick 1.0 in
  let list_neighbors = Rarray.get adjacency_list (Rnat.of_int vertex) in
  list_neighbors

let rec update_dist_all_neighbors heap list_neighbors base_dist =
  let _ = Raml.mark 2 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match list_neighbors with
    | [] -> ()
    | (vertex, dist) :: tl ->
        let _ = decrease_key heap vertex (add_distances base_dist dist) in
        update_dist_all_neighbors heap tl base_dist
  in
  let _ = Raml.mark 2 (-1.0) in
  result

let rec repeatedly_get_min_node adjacency_list heap acc =
  let _ = Raml.tick 1.0 in
  let _, length, _ = heap in
  if Rnat.to_int length = 0 then acc
  else
    let min_node = get_min heap in
    let heap_updated = delete_min heap in
    let (Vertex_dist (vertex, dist)) = min_node in
    let list_neighbors = extract_neighbors adjacency_list vertex in
    let _ = Raml.activate_counter_variable 2 in
    let _ = update_dist_all_neighbors heap list_neighbors dist in
    let _ = Raml.record_counter_variable 2 in
    let acc_updated = (vertex, dist) :: acc in
    repeatedly_get_min_node adjacency_list heap_updated acc_updated

let dijkstra_algorithm adjacency_list =
  let _ = Raml.tick 1.0 in
  let heap = construct_initial_heap adjacency_list in
  repeatedly_get_min_node adjacency_list heap []
