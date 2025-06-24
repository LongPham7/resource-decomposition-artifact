exception Invalid_input

(* Counter-related functions. We have two counters. The first counter tracks the
   recursion depth of the function splay_insert in the function
   splay_tree_insert. The second counter tracks the recursion depth of the
   function splay_lookup in the function splay_tree_lookup. *)

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

(* Splay tree *)

type splay_tree = Leaf | Node of int * splay_tree * splay_tree

let extract_value_and_subtrees tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> raise Invalid_input
  | Node (x, left, right) -> (x, left, right)

let rec splay_insert x tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_first_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (Leaf, new_counter)
    | Node (y, l, r) -> (
        if y = x then (tree, new_counter)
        else if x < y then
          match l with
          | Leaf -> (tree, new_counter)
          | Node (z, ll, rr) -> (
              if x = z then (Node (z, ll, Node (y, rr, r)), new_counter)
              else if x < z then
                match ll with
                | Leaf -> (Node (z, ll, Node (y, rr, r)), new_counter)
                | _ ->
                    let ll_splayed, counter1 = splay_insert x ll new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees ll_splayed
                    in
                    ( Node (newV, newL, Node (z, newR, Node (y, rr, r))),
                      counter1 )
              else
                match rr with
                | Leaf -> (Node (z, ll, Node (y, rr, r)), new_counter)
                | _ ->
                    let rr_splayed, counter1 = splay_insert x rr new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees rr_splayed
                    in
                    ( Node (newV, Node (z, ll, newL), Node (y, newR, r)),
                      counter1 ) )
        else
          match r with
          | Leaf -> (tree, new_counter)
          | Node (z, ll, rr) -> (
              if x = z then (Node (z, Node (y, l, ll), rr), new_counter)
              else if x < z then
                match ll with
                | Leaf -> (Node (z, Node (y, l, ll), rr), new_counter)
                | _ ->
                    let ll_splayed, counter1 = splay_insert x ll new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees ll_splayed
                    in
                    ( Node (newV, Node (y, l, newL), Node (z, newR, rr)),
                      counter1 )
              else
                match rr with
                | Leaf -> (Node (z, Node (y, l, ll), rr), new_counter)
                | _ ->
                    let rr_splayed, counter1 = splay_insert x rr new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees rr_splayed
                    in
                    ( Node (newV, Node (z, Node (y, l, ll), newL), newR),
                      counter1 ) ) )
  in
  (result, increment_first_counter counter_final)

let splay_tree_insert x tree current_original_counters =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> (Node (x, Leaf, Leaf), current_original_counters)
  | _ ->
      let initialized_counter =
        initialize_first_counter current_original_counters
      in
      let tree_splayed, counter1 = splay_insert x tree initialized_counter in
      let counter2 = set_first_counter_to_zero counter1 in
      let y, l, r = extract_value_and_subtrees tree_splayed in
      if x = y then (Node (y, l, r), counter2)
      else if x < y then (Node (x, l, Node (y, Leaf, r)), counter2)
      else (Node (x, Node (y, l, Leaf), r), counter2)

let rec splay_lookup x tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_second_counter current_original_counters in
  let result, counter_final =
    match tree with
    | Leaf -> (Leaf, new_counter)
    | Node (y, l, r) -> (
        if y = x then (tree, new_counter)
        else if x < y then
          match l with
          | Leaf -> (tree, new_counter)
          | Node (z, ll, rr) -> (
              if x = z then (Node (z, ll, Node (y, rr, r)), new_counter)
              else if x < z then
                match ll with
                | Leaf -> (Node (z, ll, Node (y, rr, r)), new_counter)
                | _ ->
                    let ll_splayed, counter1 = splay_lookup x ll new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees ll_splayed
                    in
                    ( Node (newV, newL, Node (z, newR, Node (y, rr, r))),
                      counter1 )
              else
                match rr with
                | Leaf -> (Node (z, ll, Node (y, rr, r)), new_counter)
                | _ ->
                    let rr_splayed, counter1 = splay_lookup x rr new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees rr_splayed
                    in
                    ( Node (newV, Node (z, ll, newL), Node (y, newR, r)),
                      counter1 ) )
        else
          match r with
          | Leaf -> (tree, new_counter)
          | Node (z, ll, rr) -> (
              if x = z then (Node (z, Node (y, l, ll), rr), new_counter)
              else if x < z then
                match ll with
                | Leaf -> (Node (z, Node (y, l, ll), rr), new_counter)
                | _ ->
                    let ll_splayed, counter1 = splay_lookup x ll new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees ll_splayed
                    in
                    ( Node (newV, Node (y, l, newL), Node (z, newR, rr)),
                      counter1 )
              else
                match rr with
                | Leaf -> (Node (z, Node (y, l, ll), rr), new_counter)
                | _ ->
                    let rr_splayed, counter1 = splay_lookup x rr new_counter in
                    let newV, newL, newR =
                      extract_value_and_subtrees rr_splayed
                    in
                    ( Node (newV, Node (z, Node (y, l, ll), newL), newR),
                      counter1 ) ) )
  in
  (result, increment_second_counter counter_final)

let splay_tree_lookup v tree current_original_counters =
  let _ = Raml.tick 1.0 in
  let initialized_counter =
    initialize_second_counter current_original_counters
  in
  let tree_splayed, counter1 = splay_lookup v tree initialized_counter in
  let counter2 = set_second_counter_to_zero counter1 in
  let is_found =
    match tree_splayed with
    | Leaf -> false
    | Node (x, _, _) -> if x = v then true else false
  in
  ((is_found, tree_splayed), counter2)

let rec splay_tree_repeated_insert xs acc current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (acc, current_original_counters)
  | hd :: tl ->
      let acc_updated, counter1 =
        splay_tree_insert hd acc current_original_counters
      in
      splay_tree_repeated_insert tl acc_updated counter1

let rec splay_tree_repeated_lookup xs tree current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (([], tree), current_original_counters)
  | hd :: tl ->
      let (is_found, tree_updated), counter1 =
        splay_tree_lookup hd tree current_original_counters
      in
      let (recursive_result, tree_final), counter2 =
        splay_tree_repeated_lookup tl tree_updated counter1
      in
      ((is_found :: recursive_result, tree_final), counter2)

(* Polynomial degree for AARA: 2 *)

let splay_tree_main xs1 xs2 current_original_counters =
  let _ = Raml.tick 1.0 in
  let tree, counter1 =
    splay_tree_repeated_insert xs1 Leaf current_original_counters
  in
  splay_tree_repeated_lookup xs2 tree counter1
