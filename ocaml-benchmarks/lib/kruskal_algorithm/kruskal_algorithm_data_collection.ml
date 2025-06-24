exception Invalid_input

type rank = Rank of int
type vertex = Vertex of int
type elem = Link of vertex | Root of rank * vertex

let make (v : int) : elem =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  Root (Rank 0, Vertex v)

let rec find (x : vertex) (array_vertices : elem Rarray.t) =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let (Vertex v_int) = x in
  let v_element = Rarray.get array_vertices (Rnat.of_int v_int) in
  match v_element with
  | Root (_, v) -> v
  | Link (Vertex v_parent_int) ->
      let rep = find (Vertex v_parent_int) array_vertices in
      let _ = Rarray.set array_vertices (Rnat.of_int v_int) (Link rep) in
      rep

let eq (x : vertex) (y : vertex) (array_vertices : elem Rarray.t) : bool =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let (Vertex x_rep_int) = find x array_vertices in
  let (Vertex y_rep_int) = find y array_vertices in
  x_rep_int = y_rep_int

let link (x : vertex) (y : vertex) (array_vertices : elem Rarray.t) : vertex =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let (Vertex x_int) = x in
  let (Vertex y_int) = y in
  if x_int = y_int then x
  else
    let x_element = Rarray.get array_vertices (Rnat.of_int x_int) in
    let y_element = Rarray.get array_vertices (Rnat.of_int y_int) in
    match (x_element, y_element) with
    | Root (Rank rx, vx), Root (Rank ry, _) ->
        if rx < ry then
          let _ = Rarray.set array_vertices (Rnat.of_int x_int) (Link y) in
          y
        else if rx > ry then
          let _ = Rarray.set array_vertices (Rnat.of_int y_int) (Link x) in
          x
        else
          let _ = Rarray.set array_vertices (Rnat.of_int y_int) (Link x) in
          let _ =
            Rarray.set array_vertices (Rnat.of_int x_int)
              (Root (Rank (rx + 1), vx))
          in
          x
    | _, _ -> raise Invalid_input

let union (x : vertex) (y : vertex) (array_vertices : elem Rarray.t) : vertex =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let x_rep = find x array_vertices in
  let y_rep = find y array_vertices in
  link x_rep y_rep array_vertices

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

let rec merge_sort list_edges =
  let _ = Raml.mark 1 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match list_edges with
    | [] -> []
    | [ x ] -> [ x ]
    | _ ->
        let lower, upper = split list_edges in
        let lower_sorted = merge_sort lower in
        let upper_sorted = merge_sort upper in
        merge lower_sorted upper_sorted
  in
  let _ = Raml.mark 1 (-1.0) in
  result

let rec list_length xs =
  let _ = Raml.tick 1.0 in
  match xs with [] -> 0 | _ :: tl -> 1 + list_length tl

let rec initialize_union_find_help adjacency_list array_vertices =
  let _ = Raml.tick 1.0 in
  match adjacency_list with
  | [] -> ()
  | (v, _) :: tl ->
      let v_elem = make v in
      let _ = Rarray.set array_vertices (Rnat.of_int v) v_elem in
      initialize_union_find_help tl array_vertices

let initialize_union_find adjacency_list =
  let _ = Raml.tick 1.0 in
  let num_vertices = list_length adjacency_list in
  let array_vertices =
    Rarray.make (Rnat.of_int num_vertices) (Link (Vertex 0))
  in
  let _ = initialize_union_find_help adjacency_list array_vertices in
  array_vertices

let rec traverse_sorted_list_edges list_edges array_vertices acc =
  let _ = Raml.tick 1.0 in
  match list_edges with
  | [] -> acc
  | (v1, v2, w) :: tl ->
      if eq (Vertex v1) (Vertex v2) array_vertices then
        traverse_sorted_list_edges tl array_vertices acc
      else
        let _ = union (Vertex v1) (Vertex v2) array_vertices in
        traverse_sorted_list_edges tl array_vertices ((v1, v2, w) :: acc)

let kruskal_algorithm adjacency_list =
  let _ = Raml.activate_counter_variable 0 in
  let _ = Raml.tick 1.0 in
  let list_edges = concat_list_edges adjacency_list in
  let _ = Raml.activate_counter_variable 1 in
  let sorted_list_edges = merge_sort list_edges in
  let _ = Raml.record_counter_variable 1 in
  let array_vertices = initialize_union_find adjacency_list in
  let selected_edges =
    traverse_sorted_list_edges sorted_list_edges array_vertices []
  in
  let _ = Raml.record_counter_variable 0 in
  selected_edges
