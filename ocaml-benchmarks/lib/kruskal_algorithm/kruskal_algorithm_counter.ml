exception Invalid_input

(* Counter-related functions. *)

let decrement_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  match current_counter with
  | [] -> raise Invalid_input
  | _ :: counter_tl -> (counter_tl, original_counter)

let decrement_first_counter current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (decrement_counter current_original_counters1, current_original_counters2)

let decrement_second_counter current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (current_original_counters1, decrement_counter current_original_counters2)

let increment_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  (1 :: current_counter, original_counter)

let increment_first_counter current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (increment_counter current_original_counters1, current_original_counters2)

let increment_second_counter current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (current_original_counters1, increment_counter current_original_counters2)

let initialize_counter current_original_counters =
  let _, original_counter = current_original_counters in
  (original_counter, original_counter)

let initialize_first_counter current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (initialize_counter current_original_counters1, current_original_counters2)

let initialize_second_counter current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (current_original_counters1, initialize_counter current_original_counters2)

let set_counter_to_zero current_original_counters =
  let _, original_counter = current_original_counters in
  ([], original_counter)

let set_first_counter_to_zero current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (set_counter_to_zero current_original_counters1, current_original_counters2)

let set_second_counter_to_zero current_original_counters =
  let current_original_counters1, current_original_counters2 =
    current_original_counters
  in
  (current_original_counters1, set_counter_to_zero current_original_counters2)

(* Kruskal's algorithm *)

type rank = Rank of int
type vertex = Vertex of int
type elem = Link of vertex | Root of rank * vertex

let make (v : int) current_original_counters =
  let new_counter = decrement_first_counter current_original_counters in
  let _ = Raml.tick 1.0 in
  let result = Root (Rank 0, Vertex v) in
  (result, new_counter)

let rec find (x : vertex) (array_vertices : elem Rarray.t)
    current_original_counters =
  let new_counter = decrement_first_counter current_original_counters in
  let _ = Raml.tick 1.0 in
  let (Vertex v_int) = x in
  let v_element = Rarray.get array_vertices (Rnat.of_int v_int) in
  match v_element with
  | Root (_, v) -> (v, new_counter)
  | Link (Vertex v_parent_int) ->
      let rep, final_counter =
        find (Vertex v_parent_int) array_vertices new_counter
      in
      let _ = Rarray.set array_vertices (Rnat.of_int v_int) (Link rep) in
      (rep, final_counter)

let eq (x : vertex) (y : vertex) (array_vertices : elem Rarray.t)
    current_original_counters =
  let new_counter = decrement_first_counter current_original_counters in
  let _ = Raml.tick 1.0 in
  let Vertex x_rep_int, counter1 = find x array_vertices new_counter in
  let Vertex y_rep_int, counter2 = find y array_vertices counter1 in
  let result = x_rep_int = y_rep_int in
  (result, counter2)

let link (x : vertex) (y : vertex) (array_vertices : elem Rarray.t)
    current_original_counters =
  let new_counter = decrement_first_counter current_original_counters in
  let _ = Raml.tick 1.0 in
  let (Vertex x_int) = x in
  let (Vertex y_int) = y in
  if x_int = y_int then (x, new_counter)
  else
    let x_element = Rarray.get array_vertices (Rnat.of_int x_int) in
    let y_element = Rarray.get array_vertices (Rnat.of_int y_int) in
    match (x_element, y_element) with
    | Root (Rank rx, vx), Root (Rank ry, _) ->
        if rx < ry then
          let _ = Rarray.set array_vertices (Rnat.of_int x_int) (Link y) in
          (y, new_counter)
        else if rx > ry then
          let _ = Rarray.set array_vertices (Rnat.of_int y_int) (Link x) in
          (x, new_counter)
        else
          let _ = Rarray.set array_vertices (Rnat.of_int y_int) (Link x) in
          let _ =
            Rarray.set array_vertices (Rnat.of_int x_int)
              (Root (Rank (rx + 1), vx))
          in
          (x, new_counter)
    | _, _ -> raise Invalid_input

let union (x : vertex) (y : vertex) (array_vertices : elem Rarray.t)
    current_original_counters =
  let new_counter = decrement_first_counter current_original_counters in
  let _ = Raml.tick 1.0 in
  let x_rep, counter1 = find x array_vertices new_counter in
  let y_rep, counter2 = find y array_vertices counter1 in
  link x_rep y_rep array_vertices counter2

(* Kruskal's algorithm *)

let rec append_list_edges vertex xs ys =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ys
  | (neighbor, weight) :: tl ->
      (vertex, neighbor, weight) :: append_list_edges vertex tl ys

let rec concat_list_edges adjacency_list =
  let _ = Raml.tick 1.0 in
  match adjacency_list with
  | [] -> []
  | (vertex, hg_neighbor_list) :: tl ->
      let tl_list_edges = concat_list_edges tl in
      append_list_edges vertex hg_neighbor_list tl_list_edges

let rec split xs =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], [])
  | [ x ] -> ([ x ], [])
  | x1 :: x2 :: tl ->
      let lower, upper = split tl in
      (x1 :: lower, x2 :: upper)

let rec merge xs ys =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ys
  | (xv1, xv2, xw) :: xs_tl -> (
      match ys with
      | [] -> ys
      | (yv1, yv2, yw) :: ys_tl ->
          if (xw : float) <= (yw : float) then (xv1, xv2, xw) :: merge xs_tl ys
          else (yv1, yv2, yw) :: merge xs ys_tl)

let rec merge_sort list_edges current_original_counters =
  let new_counter = decrement_second_counter current_original_counters in
  let _ = Raml.tick 1.0 in
  let result, final_counter =
    match list_edges with
    | [] -> ([], new_counter)
    | [ x ] -> ([ x ], new_counter)
    | _ ->
        let lower, upper = split list_edges in
        let lower_sorted, counter1 = merge_sort lower new_counter in
        let upper_sorted, counter2 = merge_sort upper counter1 in
        (merge lower_sorted upper_sorted, counter2)
  in
  (result, increment_second_counter final_counter)

let rec list_length xs =
  let _ = Raml.tick 1.0 in
  match xs with [] -> 0 | _ :: tl -> 1 + list_length tl

let rec initialize_union_find_help adjacency_list array_vertices
    current_original_counters =
  let _ = Raml.tick 1.0 in
  match adjacency_list with
  | [] -> ((), current_original_counters)
  | (v, _) :: tl ->
      let v_elem, final_counter = make v current_original_counters in
      let _ = Rarray.set array_vertices (Rnat.of_int v) v_elem in
      initialize_union_find_help tl array_vertices final_counter

let initialize_union_find adjacency_list current_original_counters =
  let _ = Raml.tick 1.0 in
  let num_vertices = list_length adjacency_list in
  let array_vertices =
    Rarray.make (Rnat.of_int num_vertices) (Link (Vertex 0))
  in
  let _, final_counter =
    initialize_union_find_help adjacency_list array_vertices
      current_original_counters
  in
  (array_vertices, final_counter)

let rec traverse_sorted_list_edges list_edges array_vertices acc
    current_original_counters =
  let _ = Raml.tick 1.0 in
  match list_edges with
  | [] -> (acc, current_original_counters)
  | (v1, v2, w) :: tl ->
      let eq_result, counter1 =
        eq (Vertex v1) (Vertex v2) array_vertices current_original_counters
      in
      if eq_result then
        traverse_sorted_list_edges tl array_vertices acc counter1
      else
        let _, counter2 =
          union (Vertex v1) (Vertex v2) array_vertices counter1
        in
        traverse_sorted_list_edges tl array_vertices ((v1, v2, w) :: acc)
          counter2

(* Polynomial degree for AARA: 3 *)

let kruskal_algorithm adjacency_list current_original_counters =
  let initialized_counter1 =
    initialize_first_counter current_original_counters
  in
  let initialized_counter2 = initialize_second_counter initialized_counter1 in
  let _ = Raml.tick 1.0 in
  let list_edges = concat_list_edges adjacency_list in
  let sorted_list_edges, counter1 =
    merge_sort list_edges initialized_counter2
  in
  let array_vertices, counter2 =
    initialize_union_find adjacency_list counter1
  in
  let selected_edges, _ =
    traverse_sorted_list_edges sorted_list_edges array_vertices [] counter2
  in
  selected_edges
