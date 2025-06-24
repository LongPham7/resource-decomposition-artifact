open Core

(* Conversion from JSON data structures to OCaml data structures *)

let convert_json_to_integer_list json =
  let open Yojson.Basic.Util in
  json |> to_list |> filter_int

let convert_json_to_two_dimensional_integer_list json =
  let open Yojson.Basic.Util in
  json |> to_list |> List.map ~f:convert_json_to_integer_list

let convert_json_to_list_tree_lookup_inputs json =
  let open Yojson.Basic.Util in
  let convert_json_to_tree_lookup_input json =
    let list_tree_insertions =
      json |> member "tree" |> convert_json_to_integer_list
    in
    let list_tree_lookups =
      json |> member "lookup" |> convert_json_to_integer_list
    in
    (list_tree_insertions, list_tree_lookups)
  in
  json |> to_list |> List.map ~f:convert_json_to_tree_lookup_input

let convert_json_to_list_graphs_list json =
  let open Yojson.Basic.Util in
  let convert_json_to_edge json_edge =
    let neighbor = json_edge |> member "neighbor" |> to_int in
    let weight = json_edge |> member "weight" |> to_float in
    (neighbor, weight)
  in
  let convert_json_to_list_neighbors json_list_neighbors =
    let vertex = json_list_neighbors |> member "node" |> to_int in
    let list_neighbors =
      json_list_neighbors |> member "neighbors" |> to_list
      |> List.map ~f:convert_json_to_edge
    in
    (vertex, list_neighbors)
  in
  let convert_json_to_graph json =
    json |> to_list |> List.map ~f:convert_json_to_list_neighbors
  in
  json |> to_list |> List.map ~f:convert_json_to_graph

let convert_json_to_list_graphs_array json =
  let open Yojson.Basic.Util in
  let convert_json_to_edge json_edge =
    let neighbor = json_edge |> member "neighbor" |> to_int in
    let weight = json_edge |> member "weight" |> to_float in
    (neighbor, weight)
  in
  let convert_json_to_list_neighbors array json_list_neighbors =
    let vertex = json_list_neighbors |> member "node" |> to_int in
    let list_neighbors =
      json_list_neighbors |> member "neighbors" |> to_list
      |> List.map ~f:convert_json_to_edge
    in
    Array.set array vertex list_neighbors
  in
  let convert_json_to_graph json =
    let list_json_list_neighbors = json |> to_list in
    let num_vertices = List.length list_json_list_neighbors in
    let adjacency_list = Array.create ~len:num_vertices [] in
    let _ =
      List.iter list_json_list_neighbors
        ~f:(convert_json_to_list_neighbors adjacency_list)
    in
    adjacency_list
  in
  json |> to_list |> List.map ~f:convert_json_to_graph

(* Convert OCaml data structures to JSON data structures *)

let convert_integer_list_to_json list =
  `List (List.map list ~f:(fun x -> `Int x))

let convert_two_dimensional_integer_list_to_json list =
  let list_json_of_inner_lists =
    List.map list ~f:convert_integer_list_to_json
  in
  `List list_json_of_inner_lists

let convert_float_list_to_json list =
  `List (List.map list ~f:(fun x -> `Float x))

let convert_two_dimensional_float_list_to_json list =
  let list_json_of_inner_lists = List.map list ~f:convert_float_list_to_json in
  `List list_json_of_inner_lists

let convert_three_dimensional_float_list_to_json list =
  let list_json_of_inner_lists =
    List.map list ~f:(fun inner_list ->
        convert_two_dimensional_float_list_to_json inner_list)
  in
  `List list_json_of_inner_lists

let convert_integer_pair_list_to_json list =
  `List (List.map list ~f:(fun (x, y) -> `List [ `Int x; `Int y ]))

(* Categorize a raw list of counter values according to their associated
   counters *)

let categorize_counter_values_single_round num_counters counter_values =
  let array_list_counter_values = Array.create ~len:num_counters [] in
  let insert_into_acc acc (index, value) =
    acc.(index) <- value :: acc.(index)
  in
  let _ =
    List.iter counter_values ~f:(insert_into_acc array_list_counter_values)
  in
  Array.to_list array_list_counter_values

let categorize_list_counter_values num_counters list_counter_values =
  let list_counter_values_categorized_within_round =
    List.map list_counter_values
      ~f:(categorize_counter_values_single_round num_counters)
  in
  let insert_into_acc counter_values_single_round acc =
    let zipped_acc = List.zip_exn counter_values_single_round acc in
    List.map zipped_acc ~f:(fun (value, acc) -> value :: acc)
  in
  List.fold_right list_counter_values_categorized_within_round
    ~f:insert_into_acc
    ~init:(List.init num_counters ~f:(fun _ -> []))

(* Read a two-dimensional integer list from JSON *)

let read_two_dimensional_integer_list_from_json filename =
  let json_input_data = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  json_input_data |> member "input_data"
  |> convert_json_to_two_dimensional_integer_list

(* Read a list of inputs, where each input consists of two lists, from JSON. The
   first list is used to construct a tree and the second list is used for
   repeated lookups. *)

let read_list_tree_lookup_inputs_from_json filename =
  let json_input_data = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  json_input_data |> member "input_data"
  |> convert_json_to_list_tree_lookup_inputs

(* Read a list of graphs (where the adjacency list is implemented by a list of
   lists) from JSON *)

let read_list_graphs_list_from_json filename =
  let json_input_data = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  json_input_data |> member "input_data" |> convert_json_to_list_graphs_list

(* Read a list of graphs (where the adjacency list is implemented by an array of
   lists) from JSON *)

let read_list_graphs_array_from_json filename =
  let json_input_data = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  json_input_data |> member "input_data" |> convert_json_to_list_graphs_array

(* Write to JSON *)

let write_runtime_data_one_dimensional_inputs_to_json_list runtime_data filename
    =
  let input_sizes, total_costs, (num_counters, counter_values) = runtime_data in
  let json_input_sizes = convert_integer_list_to_json input_sizes in
  let json_total_costs = convert_float_list_to_json total_costs in
  let json_counter_values =
    counter_values
    |> categorize_list_counter_values num_counters
    |> convert_three_dimensional_float_list_to_json
  in
  let json_record =
    `Assoc
      [
        ("input_sizes", json_input_sizes);
        ("total_costs", json_total_costs);
        ("num_counters", `Int num_counters);
        ("counter_values", json_counter_values);
      ]
  in
  Yojson.Basic.to_file filename json_record

let write_runtime_data_two_dimensional_inputs_to_json_list runtime_data filename
    =
  let input_sizes, total_costs, (num_counters, counter_values) = runtime_data in
  let json_input_sizes = convert_integer_pair_list_to_json input_sizes in
  let json_total_costs = convert_float_list_to_json total_costs in
  let json_counter_values =
    counter_values
    |> categorize_list_counter_values num_counters
    |> convert_three_dimensional_float_list_to_json
  in
  let json_record =
    `Assoc
      [
        ("input_sizes", json_input_sizes);
        ("total_costs", json_total_costs);
        ("num_counters", `Int num_counters);
        ("counter_values", json_counter_values);
      ]
  in
  Yojson.Basic.to_file filename json_record
