exception Invalid_input

(* Counter-related functions. We have three counters: (i) the first counter for
   tracking the recursion depth of the function heapify inside the function
   delete_min, (ii) the second counter for tracking the recursion depth of the
   function decrease_key_helper inside the function decrease_key, and (iii) the
   third counter for tracking the recursion depth of the function
   update_dist_all_neighbors inside the function repeatedly_get_min_node. *)

let decrement_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  match current_counter with
  | [] -> raise Invalid_input
  | _ :: counter_tl -> (counter_tl, original_counter)

let decrement_first_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( decrement_counter current_original_counters1,
    current_original_counters2,
    current_original_counters3 )

let decrement_second_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    decrement_counter current_original_counters2,
    current_original_counters3 )

let decrement_third_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    current_original_counters2,
    decrement_counter current_original_counters3 )

let increment_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  (1 :: current_counter, original_counter)

let increment_first_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( increment_counter current_original_counters1,
    current_original_counters2,
    current_original_counters3 )

let increment_second_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    increment_counter current_original_counters2,
    current_original_counters3 )

let increment_third_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    current_original_counters2,
    increment_counter current_original_counters3 )

let initialize_counter current_original_counters =
  let _, original_counter = current_original_counters in
  (original_counter, original_counter)

let initialize_first_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( initialize_counter current_original_counters1,
    current_original_counters2,
    current_original_counters3 )

let initialize_second_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    initialize_counter current_original_counters2,
    current_original_counters3 )

let initialize_third_counter three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    current_original_counters2,
    initialize_counter current_original_counters3 )

let set_counter_to_zero current_original_counters =
  let _, original_counter = current_original_counters in
  ([], original_counter)

let set_first_counter_to_zero three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( set_counter_to_zero current_original_counters1,
    current_original_counters2,
    current_original_counters3 )

let set_second_counter_to_zero three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    set_counter_to_zero current_original_counters2,
    current_original_counters3 )

let set_third_counter_to_zero three_current_original_counters =
  let ( current_original_counters1,
        current_original_counters2,
        current_original_counters3 ) =
    three_current_original_counters
  in
  ( current_original_counters1,
    current_original_counters2,
    set_counter_to_zero current_original_counters3 )

(* Dijkstra's algorithm *)

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

let rec heapify heap (index : int) three_current_original_counters =
  let new_counter = decrement_first_counter three_current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
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
    if smallest_index_right = index then ((), new_counter)
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
      heapify heap smallest_index_right new_counter
  in
  (result, increment_first_counter counter_final)

let get_min heap =
  let _ = Raml.tick 1.0 in
  let array, _, _ = heap in
  Rarray.get array Rnat.zero

let delete_min heap three_current_original_counters =
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
      let initialized_counter =
        initialize_first_counter three_current_original_counters
      in
      let _, counter_final = heapify heap 0 initialized_counter in
      ( (array, length_minus_one, map_vertices_to_indices),
        set_first_counter_to_zero counter_final ))

let rec decrease_key_helper heap index three_current_original_counters =
  let new_counter = decrement_second_counter three_current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
    let array, _, map_vertices_to_indices = heap in
    if index = 0 then ((), new_counter)
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
        decrease_key_helper heap parent_index new_counter
      else ((), new_counter)
  in
  (result, increment_second_counter counter_final)

let decrease_key heap vertex new_dist three_current_original_counters =
  let _ = Raml.tick 1.0 in
  let array, _, map_vertices_to_indices = heap in
  let array_index = Rarray.get map_vertices_to_indices (Rnat.of_int vertex) in
  if array_index = -1 then ((), three_current_original_counters)
  else
    let (Vertex_dist (_, dist)) = Rarray.get array (Rnat.of_int array_index) in
    if is_shorter_distance new_dist dist then
      let _ =
        Rarray.set array (Rnat.of_int array_index)
          (Vertex_dist (vertex, new_dist))
      in
      let initialized_counter =
        initialize_second_counter three_current_original_counters
      in
      let result, counter_final =
        decrease_key_helper heap array_index initialized_counter
      in
      (result, set_second_counter_to_zero counter_final)
    else ((), three_current_original_counters)

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

let rec update_dist_all_neighbors heap list_neighbors base_dist
    three_current_original_counters =
  let new_counter = decrement_third_counter three_current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
    match list_neighbors with
    | [] -> ((), new_counter)
    | (vertex, dist) :: tl ->
        let _, counter1 =
          decrease_key heap vertex (add_distances base_dist dist) new_counter
        in
        update_dist_all_neighbors heap tl base_dist counter1
  in
  (result, increment_third_counter counter_final)

let rec repeatedly_get_min_node adjacency_list heap acc
    three_current_original_counters =
  let _ = Raml.tick 1.0 in
  let _, length, _ = heap in
  if Rnat.to_int length = 0 then acc
  else
    let min_node = get_min heap in
    let heap_updated, counter1 =
      delete_min heap three_current_original_counters
    in
    let (Vertex_dist (vertex, dist)) = min_node in
    let list_neighbors = extract_neighbors adjacency_list vertex in
    let initialized_counter = initialize_third_counter counter1 in
    let _, counter2 =
      update_dist_all_neighbors heap list_neighbors dist initialized_counter
    in
    let counter3 = set_third_counter_to_zero counter2 in
    let acc_updated = (vertex, dist) :: acc in
    repeatedly_get_min_node adjacency_list heap_updated acc_updated counter3

(* Polynomial degree for AARA: 3 *)

let dijkstra_algorithm adjacency_list three_current_original_counters =
  let _ = Raml.tick 1.0 in
  let heap = construct_initial_heap adjacency_list in
  repeatedly_get_min_node adjacency_list heap [] three_current_original_counters
