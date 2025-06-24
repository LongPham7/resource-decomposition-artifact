exception Invalid_input

(* Counter-related functions. We have two counters. The first counter tracks the
   recursion depth of the function avl_tree_insert in the function
   avl_tree_repeated_insert. The second counter tracks the recursion depth of
   the function avl_tree_lookup in the function avl_tree_repeated_lookup. *)

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

(* AVL tree *)

type avl_tree = Leaf | Node of int * int * avl_tree * avl_tree

let rec avl_tree_lookup v tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_second_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (false, new_counter)
    | Node (x, _, l, r) ->
        if x = v then (true, new_counter)
        else if v < x then avl_tree_lookup v l new_counter
        else avl_tree_lookup v r new_counter
  in
  (result, increment_second_counter counter_final)

let depth tree =
  let _ = Raml.tick 1.0 in
  match tree with Node (_, d, _, _) -> d | Leaf -> 0

let value tree =
  let _ = Raml.tick 1.0 in
  match tree with Node (x, _, _, _) -> x | Leaf -> raise Invalid_input

let max (x : int) (y : int) =
  let _ = Raml.tick 1.0 in
  if x >= y then x else y

let balanceLL tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Node (x, _, Node (xl, _, ll, rl), r) ->
      let rmax = max (depth rl) (depth r) + 1 in
      let cmax = max rmax (depth ll) + 1 in
      Node (xl, cmax, ll, Node (x, rmax, rl, r))
  | _ -> raise Invalid_input

let balanceLR tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Node (x, _, Node (y, _, ll, Node (z, _, lrl, lrr)), r) ->
      let lmax = max (depth ll) (depth lrl) + 1 in
      let rmax = max (depth lrr) (depth r) + 1 in
      let cmax = max lmax rmax + 1 in
      Node (z, cmax, Node (y, lmax, ll, lrl), Node (x, rmax, lrr, r))
  | _ -> raise Invalid_input

let balanceRR tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Node (x, _, l, Node (xr, _, lr, rr)) ->
      let lmax = max (depth l) (depth lr) + 1 in
      let cmax = max lmax (depth rr) + 1 in
      Node (xr, cmax, Node (x, lmax, l, lr), rr)
  | _ -> raise Invalid_input

let balanceRL tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Node (x, _, l, Node (y, _, Node (z, _, rll, rlr), rr)) ->
      let lmax = max (depth l) (depth rll) + 1 in
      let rmax = max (depth rlr) (depth rr) + 1 in
      let cmax = max lmax rmax + 1 in
      Node (z, cmax, Node (x, lmax, l, rll), Node (y, rmax, rlr, rr))
  | _ -> raise Invalid_input

let rec avl_tree_insert v tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_first_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Node (x, _, l, r) ->
        if x = v then (tree, new_counter)
        else if v < x then
          let insL, counter1 = avl_tree_insert v l new_counter in
          let dl = depth insL in
          let dr = depth r in
          let bal = dl - dr in
          if bal < 2 || bal > 2 then (Node (x, max dr dl + 1, insL, r), counter1)
          else if v < value l then
            (balanceLL (Node (x, dl + 1, insL, r)), counter1)
          else if v > value l then
            (balanceLR (Node (x, dl + 1, insL, r)), counter1)
          else (tree, counter1)
        else
          let insR, counter1 = avl_tree_insert v r new_counter in
          let dr = depth insR in
          let dl = depth l in
          let bal = dl - dr in
          if bal < -2 || bal > -2 then
            (Node (x, max dr dl + 1, l, insR), counter1)
          else if v > value r then
            (balanceRR (Node (x, dr + 1, l, insR)), counter1)
          else if v < value r then
            (balanceRL (Node (x, dr + 1, l, insR)), counter1)
          else (tree, counter1)
    | Leaf -> (Node (v, 1, Leaf, Leaf), new_counter)
  in
  (result, increment_first_counter counter_final)

let rec min tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Node (x, _, Leaf, _) -> x
  | Node (_, _, l, _) -> min l
  | Leaf -> raise Invalid_input

let left_subtree tree =
  let _ = Raml.tick 1.0 in
  match tree with Node (_, _, l, _) -> l | Leaf -> raise Invalid_input

let right_subtree tree =
  let _ = Raml.tick 1.0 in
  match tree with Node (_, _, _, r) -> r | Leaf -> raise Invalid_input

let rec avl_tree_repeated_insert xs acc current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (acc, current_original_counters)
  | hd :: tl ->
      let initialized_counter =
        initialize_first_counter current_original_counters
      in
      let acc_updated, counter1 = avl_tree_insert hd acc initialized_counter in
      avl_tree_repeated_insert tl acc_updated counter1

let rec avl_tree_repeated_lookup xs tree current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], current_original_counters)
  | hd :: tl ->
      let initialized_counter =
        initialize_second_counter current_original_counters
      in
      let is_found, counter1 = avl_tree_lookup hd tree initialized_counter in
      let recursive_result, counter2 =
        avl_tree_repeated_lookup tl tree counter1
      in
      (is_found :: recursive_result, counter2)

(* Polynomial degree for AARA: 2 *)

let avl_tree_main xs1 xs2 current_original_counters =
  let _ = Raml.tick 1.0 in
  let tree, counter1 =
    avl_tree_repeated_insert xs1 Leaf current_original_counters
  in
  avl_tree_repeated_lookup xs2 tree counter1
