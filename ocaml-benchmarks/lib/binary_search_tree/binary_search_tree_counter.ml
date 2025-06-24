exception Invalid_input

(* Counter-related functions.

   In the benchmark of the unbalanced binary search tree, we use two counters.
   The first counter tracks the recursion depth of the function
   binary_search_tree_insert in the function
   binary_search_tree_repeated_insert_helper. The second counter tracks the
   recursion depth of the function binary_search_tree_lookup in the function
   binary_search_tree_repeated_lookup.

   In the benchmark of the balanced binary search tree, we use two counters. The
   first counter tracks the recursion depth of the function merge_sort in the
   function balanced_binary_search_tree_main. The second counter tracks the
   recursion depth of binary_search_tree_lookup, as in the benchmark of the
   unbalanced binary search tree. *)

let decrement_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  match current_counter with
  | [] -> raise Invalid_input
  | _ :: counter_tl -> (counter_tl, original_counter)

let decrement_first_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (decrement_counter current_original_counters1, current_original_counters2)

let decrement_second_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, decrement_counter current_original_counters2)

let increment_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  (1 :: current_counter, original_counter)

let increment_first_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (increment_counter current_original_counters1, current_original_counters2)

let increment_second_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, increment_counter current_original_counters2)

let initialize_counter current_original_counters =
  let _, original_counter = current_original_counters in
  (original_counter, original_counter)

let initialize_first_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (initialize_counter current_original_counters1, current_original_counters2)

let initialize_second_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, initialize_counter current_original_counters2)

let set_counter_to_zero current_original_counters =
  let _, original_counter = current_original_counters in
  ([], original_counter)

let set_first_counter_to_zero two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (set_counter_to_zero current_original_counters1, current_original_counters2)

let set_second_counter_to_zero two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, set_counter_to_zero current_original_counters2)

(* Merge sort *)

let rec merge (xs : int list) (ys : int list) =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ys
  | x :: xs_tl -> (
      match ys with
      | [] -> xs
      | y :: ys_tl ->
          if x <= y then x :: merge xs_tl ys else y :: merge xs ys_tl)

let rec split xs =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], [])
  | [ x ] -> ([ x ], [])
  | x1 :: x2 :: tl ->
      let lower, upper = split tl in
      (x1 :: lower, x2 :: upper)

let rec merge_sort xs current_original_counters =
  let new_counter = decrement_first_counter current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
    match xs with
    | [] -> ([], new_counter)
    | [ x ] -> ([ x ], new_counter)
    | _ ->
        let lower, upper = split xs in
        let lower_sorted, counter1 = merge_sort lower new_counter in
        let upper_sorted, counter2 = merge_sort upper counter1 in
        (merge lower_sorted upper_sorted, counter2)
  in
  (result, increment_first_counter counter_final)

(* Binary search tree *)

type binary_tree = Leaf | Node of int * binary_tree * binary_tree
type nat = Zero | Succ of nat

let rec divide_nat_by_two n =
  (* let _ = Raml.tick 1.0 in *)
  match n with
  | Zero -> (Zero, Zero)
  | Succ n -> (
      match n with
      | Zero -> (Succ Zero, Zero)
      | Succ n ->
          let n1, n2 = divide_nat_by_two n in
          (Succ n1, Succ n2))

let rec balanced_binary_tree_from_sorted_list_helper n xs =
  let _ = Raml.tick 1.0 in
  match n with
  | Zero -> (Leaf, xs)
  | Succ Zero -> (
      match xs with
      | [] -> raise Invalid_input
      | hd :: tl -> (Node (hd, Leaf, Leaf), tl))
  | Succ n_minus_one -> (
      let n1, n2 = divide_nat_by_two n_minus_one in
      let t1, xs1 = balanced_binary_tree_from_sorted_list_helper n1 xs in
      match xs1 with
      | [] -> raise Invalid_input
      | y :: ys ->
          let t2, xs2 = balanced_binary_tree_from_sorted_list_helper n2 ys in
          (Node (y, t1, t2), xs2))

let rec list_length_nat xs =
  let _ = Raml.tick 1.0 in
  match xs with [] -> Zero | _ :: tl -> Succ (list_length_nat tl)

let balanced_binary_tree_from_sorted_list xs =
  let _ = Raml.tick 1.0 in
  let n = list_length_nat xs in
  let t, _ = balanced_binary_tree_from_sorted_list_helper n xs in
  t

let rec binary_search_tree_insert v tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_first_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (Node (v, Leaf, Leaf), new_counter)
    | Node (x, left, right) ->
        if x = v then (tree, new_counter)
        else if v < x then
          let left_inserted, counter1 =
            binary_search_tree_insert v left new_counter
          in
          (Node (x, left_inserted, right), counter1)
        else
          let right_inserted, counter1 =
            binary_search_tree_insert v right new_counter
          in
          (Node (x, left, right_inserted), counter1)
  in
  (result, increment_first_counter counter_final)

let rec binary_search_tree_lookup v tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_second_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (false, new_counter)
    | Node (x, left, right) ->
        if x = v then (true, new_counter)
        else if v < x then binary_search_tree_lookup v left new_counter
        else binary_search_tree_lookup v right new_counter
  in
  (result, increment_second_counter counter_final)

let rec binary_search_tree_repeated_insert xs acc current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (acc, current_original_counters)
  | hd :: tl ->
      let initialized_counter =
        initialize_first_counter current_original_counters
      in
      let acc_updated, counter1 =
        binary_search_tree_insert hd acc initialized_counter
      in
      let counter2 = set_first_counter_to_zero counter1 in
      binary_search_tree_repeated_insert tl acc_updated counter2

let rec binary_search_tree_repeated_lookup xs tree current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], current_original_counters)
  | hd :: tl ->
      let initialized_counter =
        initialize_second_counter current_original_counters
      in
      let is_found, counter1 =
        binary_search_tree_lookup hd tree initialized_counter
      in
      let counter2 = set_second_counter_to_zero counter1 in
      let recursive_result, counter3 =
        binary_search_tree_repeated_lookup tl tree counter2
      in
      (is_found :: recursive_result, counter3)

(* Polynomial degree for AARA: 2 *)

let unbalanced_binary_search_tree_main xs1 xs2 current_original_counters =
  let _ = Raml.tick 1.0 in
  let tree, counter1 =
    binary_search_tree_repeated_insert xs1 Leaf current_original_counters
  in
  binary_search_tree_repeated_lookup xs2 tree counter1

(* Polynomial degree for AARA: 2 *)

let balanced_binary_search_tree_main xs1 xs2 current_original_counters =
  let _ = Raml.tick 1.0 in
  let counter1 = initialize_first_counter current_original_counters in
  let xs_sorted, counter2 = merge_sort xs1 counter1 in
  let counter3 = set_first_counter_to_zero counter2 in
  let tree = balanced_binary_tree_from_sorted_list xs_sorted in
  binary_search_tree_repeated_lookup xs2 tree counter3
