exception Invalid_input

(* Counter-related functions. We use two counters. The first counter tracks the
   recursion depth of the function red_black_tree_insert_helper in the function
   red_black_tree_insert. The second counter tracks the recursion depth of the
   function red_black_tree_lookup in the function
   red_black_tree_repeated_lookup. *)

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

(* Red-black tree *)

type color = Red | Black

type red_black_tree =
  | Leaf
  | Node of color * int * red_black_tree * red_black_tree

let rec red_black_tree_lookup v tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_second_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (false, new_counter)
    | Node (_, x, left, right) ->
        if x = v then (true, new_counter)
        else if v < x then red_black_tree_lookup v left new_counter
        else red_black_tree_lookup v right new_counter
  in
  (result, increment_second_counter counter_final)

let balance color v t1 t2 current_original_counters =
  let _ = Raml.tick 1.0 in
  match (color, v, t1, t2) with
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      ( Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d)),
        current_original_counters )
  | a, b, c, d -> (Node (a, b, c, d), current_original_counters)

let rec red_black_tree_insert_helper x tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_first_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (Node (Red, x, Leaf, Leaf), new_counter)
    | Node (color, y, a, b) ->
        let _ = Raml.tick 1.0 in
        if x < y then
          let result_recursive, counter1 =
            red_black_tree_insert_helper x a new_counter
          in
          balance color y result_recursive b counter1
        else if x > y then
          let result_recursive, counter1 =
            red_black_tree_insert_helper x b new_counter
          in
          balance color y a result_recursive counter1
        else (tree, new_counter)
  in
  (result, increment_first_counter counter_final)

let red_black_tree_insert x tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let initialized_counter =
    initialize_first_counter current_original_counters
  in
  let insert_result, counter1 =
    red_black_tree_insert_helper x tree initialized_counter
  in
  let counter2 = set_first_counter_to_zero counter1 in
  match insert_result with
  | Node (_, y, a, b) -> (Node (Black, y, a, b), counter2)
  | Leaf -> raise Invalid_input

(* Polynomial degree for AARA: 1 *)

let rec red_black_tree_repeated_insert xs acc current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (acc, current_original_counters)
  | hd :: tl ->
      let acc_updated, counter1 =
        red_black_tree_insert hd acc current_original_counters
      in
      red_black_tree_repeated_insert tl acc_updated counter1

let rec red_black_tree_repeated_lookup xs tree current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], current_original_counters)
  | hd :: tl ->
      let initialized_counter =
        initialize_second_counter current_original_counters
      in
      let is_found, counter1 =
        red_black_tree_lookup hd tree initialized_counter
      in
      let counter2 = set_second_counter_to_zero counter1 in
      let recursive_result, counter3 =
        red_black_tree_repeated_lookup tl tree counter2
      in
      (is_found :: recursive_result, counter3)

(* We first build a red-black tree by successively inserting the elements of the
   first input list xs1 to the tree. We then successively look up the elements
   of the second input list xs2 in the tree. *)

(* Polynomial degree for AARA: 2 *)

let red_black_tree_main xs1 xs2 current_original_counters =
  let _ = Raml.tick 1.0 in
  let tree, counter1 =
    red_black_tree_repeated_insert xs1 Leaf current_original_counters
  in
  red_black_tree_repeated_lookup xs2 tree counter1
