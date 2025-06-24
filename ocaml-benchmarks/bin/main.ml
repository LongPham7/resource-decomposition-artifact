open Core
open Data_structures
open Pretty_printer
open Json_manipulation

(* Merge sort *)

let test_merge_sort () =
  let _ = Raml.set_total_cost_to_zero () in
  let input_list = List.range 0 10 in
  let sorted_list = Merge_sort.merge_sort input_list in
  print_out_integer_list sorted_list;
  print_out_total_cost ()

let collect_data_merge_sort input_filename output_filename =
  let list_inputs =
    read_two_dimensional_integer_list_from_json input_filename
  in
  let num_counters = 1 in
  let run_benchmark xs =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = Merge_sort_data_collection.main xs in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes = List.map list_inputs ~f:(fun xs -> List.length xs) in
  write_runtime_data_one_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Quicksort *)

let test_quicksort () =
  let input_list = List.range 0 10 in
  let sorted_list = Quicksort.quicksort input_list in
  List.iter sorted_list ~f:(fun x -> printf "%d " x);
  print_string "\n"

let collect_data_quicksort input_filename output_filename =
  let list_inputs =
    read_two_dimensional_integer_list_from_json input_filename
  in
  let num_counters = 1 in
  let run_benchmark xs =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = Quicksort_data_collection.main xs in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes = List.map list_inputs ~f:(fun xs -> List.length xs) in
  write_runtime_data_one_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Bubble sort *)

let test_bubble_sort () =
  (* let input_list = List.range 0 10 in *)
  let input_list = [ 6; 7; 4; 3; 1; 2; 8; 0; 5; 9 ] in
  let sorted_list = Bubble_sort.bubble_sort input_list in
  List.iter sorted_list ~f:(fun x -> printf "%d " x);
  print_string "\n"

let collect_data_bubble_sort input_filename output_filename =
  let list_inputs =
    read_two_dimensional_integer_list_from_json input_filename
  in
  let num_counters = 1 in
  let run_benchmark xs =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = Bubble_sort_data_collection.main xs in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes = List.map list_inputs ~f:(fun xs -> List.length xs) in
  write_runtime_data_one_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Heap sort *)

let test_heap_sort () =
  (* let input_list = List.range 0 10 in *)
  let input_list = [ 6; 7; 4; 3; 1; 2; 8; 0; 5; 9 ] in
  let sorted_list = Heap_sort.heap_sort input_list in
  List.iter sorted_list ~f:(fun x -> printf "%d " x);
  print_string "\n"

let collect_data_heap_sort input_filename output_filename =
  let list_inputs =
    read_two_dimensional_integer_list_from_json input_filename
  in
  let num_counters = 2 in
  let run_benchmark xs =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = Heap_sort_data_collection.heap_sort xs in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes = List.map list_inputs ~f:(fun xs -> List.length xs) in
  write_runtime_data_one_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Huffman code *)

let test_huffman_code () =
  let open Huffman_code in
  let rec fprint_code_tree out v =
    match v with
    | LeafCode (character, count) ->
        fprintf out "LeafNode(%d, %d)" character count
    | NodeCode (count, left, right) ->
        fprintf out "NodeCode(%d, %a, %a)" count fprint_code_tree left
          fprint_code_tree right
  in
  (* This input comes from Section 15.3 on Huffman codes from the textbook
     Introduction to Algorithms fourth edition by Cormen et al. The example
     has the following pairs of characters and their frequencies: ('a', 45),
     ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5). These characters are
     represented by the integers from one to six in my implementation. *)
  let input_list = [ (1, 45); (2, 13); (3, 12); (4, 16); (5, 9); (6, 5) ] in
  let huffman_code = huffman_code input_list in
  fprint_code_tree stdout huffman_code;
  printf "\n"

let collect_data_huffman_code input_filename output_filename =
  let augment_input_with_indices list =
    List.mapi list ~f:(fun index x -> (index, x))
  in
  let list_inputs =
    input_filename |> read_two_dimensional_integer_list_from_json
    |> List.map ~f:augment_input_with_indices
  in
  let num_counters = 2 in
  let run_benchmark xs =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = Huffman_code_data_collection.huffman_code xs in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes = List.map list_inputs ~f:(fun xs -> List.length xs) in
  write_runtime_data_one_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Binary search tree *)

let test_unbalanced_binary_search_tree () =
  let open Binary_search_tree in
  let insert_list = List.range 0 10 in
  let lookup_list = [ 1; 2; 11; 12 ] in
  let result = unbalanced_binary_search_tree_main insert_list lookup_list in
  List.iter result ~f:(fun x -> printf "%b " x);
  print_string "\n"

let test_balanced_binary_search_tree () =
  let open Binary_search_tree in
  let insert_list = List.range 0 10 in
  let lookup_list = [ 1; 2; 11; 12 ] in
  let result = balanced_binary_search_tree_main insert_list lookup_list in
  List.iter result ~f:(fun x -> printf "%b " x);
  print_string "\n"

let collect_data_binary_search_tree_unbalanced input_filename output_filename =
  let open Binary_search_tree_data_collection in
  let list_inputs = input_filename |> read_list_tree_lookup_inputs_from_json in
  let num_counters = 2 in
  let run_benchmark (xs1, xs2) =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = unbalanced_binary_search_tree_main xs1 xs2 in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes =
    List.map list_inputs ~f:(fun (xs1, xs2) ->
        (List.length xs1, List.length xs2))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

let collect_data_binary_search_tree_balanced input_filename output_filename =
  let open Binary_search_tree_data_collection in
  let list_inputs = input_filename |> read_list_tree_lookup_inputs_from_json in
  let num_counters = 2 in
  let run_benchmark (xs1, xs2) =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = balanced_binary_search_tree_main xs1 xs2 in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes =
    List.map list_inputs ~f:(fun (xs1, xs2) ->
        (List.length xs1, List.length xs2))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Red-black tree *)

let test_red_black_tree () =
  let open Red_black_tree in
  let insert_list = List.range 0 10 in
  let lookup_list = [ 1; 2; 11; 12 ] in
  let result = red_black_tree_main insert_list lookup_list in
  List.iter result ~f:(fun x -> printf "%b " x);
  print_string "\n"

let collect_data_red_black_tree input_filename output_filename =
  let open Red_black_tree_data_collection in
  let list_inputs = input_filename |> read_list_tree_lookup_inputs_from_json in
  let num_counters = 2 in
  let run_benchmark (xs1, xs2) =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = red_black_tree_main xs1 xs2 in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes =
    List.map list_inputs ~f:(fun (xs1, xs2) ->
        (List.length xs1, List.length xs2))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* AVL tree *)

let test_avl_tree () =
  let open Avl_tree in
  let insert_list = List.range 0 10 in
  let lookup_list = [ 1; 2; 11; 12 ] in
  let result = avl_tree_main insert_list lookup_list in
  List.iter result ~f:(fun x -> printf "%b " x);
  print_string "\n"

let collect_data_avl_tree input_filename output_filename =
  let open Avl_tree_data_collection in
  let list_inputs = input_filename |> read_list_tree_lookup_inputs_from_json in
  let num_counters = 2 in
  let run_benchmark (xs1, xs2) =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = avl_tree_main xs1 xs2 in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes =
    List.map list_inputs ~f:(fun (xs1, xs2) ->
        (List.length xs1, List.length xs2))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Splay tree *)

let test_splay_tree () =
  let open Splay_tree in
  let insert_list = List.range 0 10 in
  let lookup_list = [ 1; 2; 11; 12 ] in
  let result, _ = splay_tree_main insert_list lookup_list in
  List.iter result ~f:(fun x -> printf "%b " x);
  print_string "\n"

let collect_data_splay_tree input_filename output_filename =
  let open Splay_tree_data_collection in
  let list_inputs = input_filename |> read_list_tree_lookup_inputs_from_json in
  let num_counters = 2 in
  let run_benchmark (xs1, xs2) =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = splay_tree_main xs1 xs2 in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  let list_input_sizes =
    List.map list_inputs ~f:(fun (xs1, xs2) ->
        (List.length xs1, List.length xs2))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Prim's algorithm *)

let test_prim_algorithm () =
  let open Prim_algorithm in
  (* This adjacency list comes from the example in Section 21.4 on Prim's
     algorithm in the textbook Introduction to Algorithms fourth edition by
     Cormen et al. The adjacency list is implemented as an array of lists. We
     should not encode it as a list of inner lists, because it would require
     non-constant time to access the list of edges incident on a vertex of
     interest, resulting in unnecessarily large time complexity. *)
  let adjacency_list = Rarray.make 9 [] in
  let _ =
    Rarray.set adjacency_list 0 [ (1, 4.); (7, 8.) ];
    Rarray.set adjacency_list 1 [ (0, 4.); (2, 8.); (7, 11.) ];
    Rarray.set adjacency_list 2 [ (1, 8.); (3, 7.); (5, 4.); (8, 2.) ];
    Rarray.set adjacency_list 3 [ (2, 7.); (4, 9.); (5, 14.) ];
    Rarray.set adjacency_list 4 [ (3, 9.); (5, 10.) ];
    Rarray.set adjacency_list 5 [ (3, 14.); (4, 10.); (2, 4.); (6, 2.) ];
    Rarray.set adjacency_list 6 [ (5, 2.); (7, 1.); (8, 6.) ];
    Rarray.set adjacency_list 7 [ (0, 8.); (1, 11.); (6, 1.); (8, 7.) ];
    Rarray.set adjacency_list 8 [ (2, 2.); (6, 6.); (7, 7.) ]
  in
  let selected_edges = prim_algorithm adjacency_list in
  List.iter selected_edges ~f:(fun (v, w) ->
      match w with
      | Infinity -> printf "Selected edge: vertex = %d dist = inf\n" v
      | Some d -> printf "Selected edge: vertex = %d dist = %.2f\n" v d)

let collect_data_prim_algorithm input_filename output_filename =
  let open Prim_algorithm_data_collection in
  let list_graphs = input_filename |> read_list_graphs_array_from_json in
  let num_counters = 3 in
  let run_benchmark adjacency_list =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = prim_algorithm adjacency_list in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_graphs ~f:run_benchmark)
  in
  let list_input_sizes =
    let max_degree adjacency_list =
      adjacency_list |> Array.to_list
      |> List.map ~f:(fun list_neighbors -> List.length list_neighbors)
      |> List.fold ~init:0 ~f:(fun acc x -> max acc x)
    in
    List.map list_graphs ~f:(fun graph ->
        (Array.length graph, max_degree graph))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Boruvka's algorithm *)

let test_boruvka_algorithm () =
  let open Boruvka_algorithm in
  (* This adjacency list comes from the example in Section 21.4 on Prim's
     algorithm in the textbook Introduction to Algorithms fourth edition by
     Cormen et al. *)
  let adjacency_list =
    [
      (0, [ (1, 4); (7, 8) ]);
      (1, [ (0, 4); (2, 8); (7, 11) ]);
      (2, [ (1, 8); (3, 7); (5, 4); (8, 2) ]);
      (3, [ (2, 7); (4, 9); (5, 14) ]);
      (4, [ (3, 9); (5, 10) ]);
      (5, [ (3, 14); (4, 10); (2, 4); (6, 2) ]);
      (6, [ (5, 2); (7, 1); (8, 6) ]);
      (7, [ (0, 8); (1, 11); (6, 1); (8, 7) ]);
      (8, [ (2, 2); (6, 6); (7, 7) ]);
    ]
  in
  let selected_edges = boruvka_algorithm adjacency_list in
  List.iter selected_edges ~f:(fun (v, d) ->
      printf "Selected edge: vertex = %d, dist = %d\n" v d)

(* Dijkstra's algorithm *)

let test_dijkstra_algorithm () =
  let open Dijkstra_algorithm in
  (* This adjacency list comes from the example in Section 22.3 on Dijkstra's
     algorithm in the textbook Introduction to Algorithms fourth edition by
     Cormen et al. *)
  let adjacency_list = Rarray.make 5 [] in
  let _ =
    Rarray.set adjacency_list 0 [ (1, 10.); (3, 5.) ];
    Rarray.set adjacency_list 1 [ (2, 1.); (3, 2.) ];
    Rarray.set adjacency_list 2 [ (4, 4.) ];
    Rarray.set adjacency_list 3 [ (1, 3.); (2, 9.); (4, 2.) ];
    Rarray.set adjacency_list 4 [ (0, 7.); (2, 6.) ]
  in
  let selected_edges = dijkstra_algorithm adjacency_list in
  List.iter selected_edges ~f:(fun (v, w) ->
      match w with
      | Infinity -> printf "Selected edge: vertex = %d dist = inf\n" v
      | Some d -> printf "Selected edge: vertex = %d dist = %.2f\n" v d)

let collect_data_dijkstra_algorithm input_filename output_filename =
  let open Dijkstra_algorithm_data_collection in
  let list_graphs = input_filename |> read_list_graphs_array_from_json in
  let num_counters = 3 in
  let run_benchmark adjacency_list =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = dijkstra_algorithm adjacency_list in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_graphs ~f:run_benchmark)
  in
  let list_input_sizes =
    let max_degree adjacency_list =
      adjacency_list |> Array.to_list
      |> List.map ~f:(fun list_neighbors -> List.length list_neighbors)
      |> List.fold ~init:0 ~f:(fun acc x -> max acc x)
    in
    List.map list_graphs ~f:(fun graph ->
        (Array.length graph, max_degree graph))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Bellman-Ford algorithm *)

let test_bellman_ford_algorithm () =
  let open Bellman_ford_algorithm in
  (* This adjacency list comes from the example in Section 22.1 on the
     Bellman-Ford algorithm in the textbook Introduction to Algorithms fourth
     edition by Cormen et al. This example does not have a negative-weight
     cycle, which is why the saturation-based Bellman-Ford algorithm implemented
     here works. Otherwise, if we had a negative-weight cycle, the algorithm
     would diverge because it is saturation-based and hence does not check for a
     negative-weight cycle. *)
  let adjacency_list =
    [
      (0, [ (1, 6.); (3, 7.) ]);
      (1, [ (2, 5.); (3, 8.); (4, -4.) ]);
      (2, [ (1, -2.) ]);
      (3, [ (2, -3.); (4, 9.) ]);
      (4, [ (0, 2.); (2, 7.) ]);
    ]
  in
  let selected_edges = bellman_ford_algorithm adjacency_list in
  List.iter selected_edges ~f:(fun (v, w) ->
      match w with
      | Infinity -> printf "Selected edge: vertex = %d dist = inf\n" v
      | Some d -> printf "Selected edge: vertex = %d dist = %.2f\n" v d)

let collect_data_bellman_ford_algorithm input_filename output_filename =
  let list_graphs = input_filename |> read_list_graphs_list_from_json in
  let num_counters = 1 in
  let run_benchmark adjacency_list =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ =
      Bellman_ford_algorithm_data_collection.bellman_ford_algorithm
        adjacency_list
    in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_graphs ~f:run_benchmark)
  in
  let list_input_sizes =
    let max_degree adjacency_list =
      adjacency_list
      |> List.map ~f:(fun (_, list_neighbors) -> List.length list_neighbors)
      |> List.fold ~init:0 ~f:(fun acc x -> max acc x)
    in
    List.map list_graphs ~f:(fun graph -> (List.length graph, max_degree graph))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Kruskal's algorithm *)

let test_kruskal_algorithm () =
  let open Kruskal_algorithm in
  let adjacency_list =
    [
      (0, [ (1, 4.); (7, 8.) ]);
      (1, [ (0, 4.); (2, 8.); (7, 11.) ]);
      (2, [ (1, 8.); (3, 7.); (5, 4.); (8, 2.) ]);
      (3, [ (2, 7.); (4, 9.); (5, 14.) ]);
      (4, [ (3, 9.); (5, 10.) ]);
      (5, [ (3, 14.); (4, 10.); (2, 4.); (6, 2.) ]);
      (6, [ (5, 2.); (7, 1.); (8, 6.) ]);
      (7, [ (0, 8.); (1, 11.); (6, 1.); (8, 7.) ]);
      (8, [ (2, 2.); (6, 6.); (7, 7.) ]);
    ]
  in
  let selected_edges = kruskal_algorithm adjacency_list in
  List.iter selected_edges ~f:(fun (v1, v2, w) ->
      printf "Selected edge: (%d, %d) weight = %.2f\n" v1 v2 w)

let collect_data_kruskal_algorithm input_filename output_filename =
  let open Kruskal_algorithm_data_collection in
  let list_graphs = input_filename |> read_list_graphs_list_from_json in
  let num_counters = 3 in
  let run_benchmark adjacency_list =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = kruskal_algorithm adjacency_list in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_graphs ~f:run_benchmark)
  in
  let list_input_sizes =
    let max_degree adjacency_list =
      adjacency_list
      |> List.map ~f:(fun (_, list_neighbors) -> List.length list_neighbors)
      |> List.fold ~init:0 ~f:(fun acc x -> max acc x)
    in
    List.map list_graphs ~f:(fun graph -> (List.length graph, max_degree graph))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

(* Quicksort for the integration TiML + data-driven analysis. In this version of
   quicksort, the cost of integer comparison is proportional to the logarithm of
   the maximum integers appearing in an input list. *)

let test_quicksort_timl () =
  let input_list = List.range 0 10 in
  let sorted_list = Quicksort_timl.quicksort input_list in
  List.iter sorted_list ~f:(fun x -> printf "%d " x);
  print_string "\n"

let collect_data_quicksort_timl input_filename output_filename =
  let list_inputs =
    read_two_dimensional_integer_list_from_json input_filename
  in
  let num_counters = 1 in
  let run_benchmark xs =
    let _ = Raml.set_total_cost_to_zero () in
    let _ = Raml.initialize_counter_variable num_counters in
    let _ = Quicksort_timl_data_collection.quicksort xs in
    let total_cost = Raml.get_total_cost () in
    let counter_values = Raml.get_counter_values () in
    (total_cost, counter_values)
  in
  let list_total_costs, list_counter_values =
    List.unzip (List.map list_inputs ~f:run_benchmark)
  in
  (* We calculate two sizes of an input list: (i) the length of the list and
     (ii) the maximum integer appearing in the list. *)
  let list_input_sizes =
    let max_list_element xs =
      List.fold xs ~init:(List.hd_exn xs) ~f:(fun acc x -> Int.max acc x)
    in
    List.map list_inputs ~f:(fun xs -> (List.length xs, max_list_element xs))
  in
  write_runtime_data_two_dimensional_inputs_to_json_list
    (list_input_sizes, list_total_costs, (num_counters, list_counter_values))
    output_filename

let test_benchmark benchmark_name =
  let benchmark_function =
    match benchmark_name with
    | "merge_sort" -> test_merge_sort
    | "quicksort" -> test_quicksort
    | "heap_sort" -> test_heap_sort
    | "bubble_sort" -> test_bubble_sort
    | "unbalanced_binary_search_tree" -> test_unbalanced_binary_search_tree
    | "balanced_binary_search_tree" -> test_balanced_binary_search_tree
    | "red_black_tree" -> test_red_black_tree
    | "avl_tree" -> test_avl_tree
    | "splay_tree" -> test_splay_tree
    | "huffman_code" -> test_huffman_code
    | "prim_algorithm" -> test_prim_algorithm
    | "boruvka_algorithm" -> test_boruvka_algorithm
    | "dijkstra_algorithm" -> test_dijkstra_algorithm
    | "bellman_ford_algorithm" -> test_bellman_ford_algorithm
    | "kruskal_algorithm" -> test_kruskal_algorithm
    | "quicksort_timl" -> test_quicksort_timl
    | _ -> raise (Invalid_argument "The given benchmark name cannot be tested")
  in
  let _ = printf "Test benchmark %s:\n" benchmark_name in
  benchmark_function ()

let collect_data_benchmark benchmark_name input_filename output_filename =
  let benchmark_function =
    match benchmark_name with
    | "merge_sort" -> collect_data_merge_sort
    | "quicksort" -> collect_data_quicksort
    | "bubble_sort" -> collect_data_bubble_sort
    | "heap_sort" -> collect_data_heap_sort
    | "huffman_code" -> collect_data_huffman_code
    | "unbalanced_binary_search_tree" ->
        collect_data_binary_search_tree_unbalanced
    | "balanced_binary_search_tree" -> collect_data_binary_search_tree_balanced
    | "red_black_tree" -> collect_data_red_black_tree
    | "avl_tree" -> collect_data_avl_tree
    | "splay_tree" -> collect_data_splay_tree
    | "prim_algorithm" -> collect_data_prim_algorithm
    | "dijkstra_algorithm" -> collect_data_dijkstra_algorithm
    | "bellman_ford_algorithm" -> collect_data_bellman_ford_algorithm
    | "kruskal_algorithm" -> collect_data_kruskal_algorithm
    | "quicksort_timl" -> collect_data_quicksort_timl
    | _ ->
        raise
          (Invalid_argument
             "The given benchmark name cannot be used for data collection")
  in
  benchmark_function input_filename output_filename

let main mode benchmark_name input_filename output_filename =
  match mode with
  | "test" -> test_benchmark benchmark_name
  | "data" ->
      let input_filename =
        Option.value_exn input_filename
          ~message:"The input filename is not provided for data collection"
      in
      let output_filename =
        Option.value_exn output_filename
          ~message:"The output filename is not provided for data collection"
      in
      collect_data_benchmark benchmark_name input_filename output_filename
  | _ -> raise (Invalid_argument "The given mode is invalid")

let command =
  Command.basic ~summary:"RaML benchmarks for Counter AARA"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map mode = anon ("mode" %: string)
     and benchmark_name = anon ("benchmark_name" %: string)
     and input_filename =
       flag "-i" (optional string) ~doc:"string Input filename"
     and output_filename =
       flag "-o" (optional string) ~doc:"string Output filename"
     in
     fun () -> main mode benchmark_name input_filename output_filename)

let () = Command_unix.run ~version:"1.0" command
