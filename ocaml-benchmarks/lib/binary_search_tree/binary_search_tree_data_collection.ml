exception Invalid_input

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

let rec merge_sort xs =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match xs with
    | [] -> []
    | [ x ] -> [ x ]
    | _ ->
        let lower, upper = split xs in
        let lower_sorted = merge_sort lower in
        let upper_sorted = merge_sort upper in
        merge lower_sorted upper_sorted
  in
  let _ = Raml.mark 0 (-1.0) in
  result

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

let rec binary_search_tree_insert v tree =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match tree with
    | Leaf -> Node (v, Leaf, Leaf)
    | Node (x, left, right) ->
        if x = v then tree
        else if v < x then
          let left_inserted = binary_search_tree_insert v left in
          Node (x, left_inserted, right)
        else
          let right_inserted = binary_search_tree_insert v right in
          Node (x, left, right_inserted)
  in
  let _ = Raml.mark 0 (-1.0) in
  result

let rec binary_search_tree_lookup v tree =
  let _ = Raml.mark 1 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match tree with
    | Leaf -> false
    | Node (x, left, right) ->
        if x = v then true
        else if v < x then binary_search_tree_lookup v left
        else binary_search_tree_lookup v right
  in
  let _ = Raml.mark 1 (-1.0) in
  result

let rec binary_search_tree_repeated_insert xs acc =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> acc
  | hd :: tl ->
      let _ = Raml.activate_counter_variable 0 in
      let acc_updated = binary_search_tree_insert hd acc in
      let _ = Raml.record_counter_variable 0 in
      binary_search_tree_repeated_insert tl acc_updated

let rec binary_search_tree_repeated_lookup xs tree =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> []
  | hd :: tl ->
      let _ = Raml.activate_counter_variable 1 in
      let is_found = binary_search_tree_lookup hd tree in
      let _ = Raml.record_counter_variable 1 in
      let recursive_result = binary_search_tree_repeated_lookup tl tree in
      is_found :: recursive_result

let unbalanced_binary_search_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let tree = binary_search_tree_repeated_insert xs1 Leaf in
  binary_search_tree_repeated_lookup xs2 tree

let balanced_binary_search_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let _ = Raml.activate_counter_variable 0 in
  let xs_sorted = merge_sort xs1 in
  let _ = Raml.record_counter_variable 0 in
  let tree = balanced_binary_tree_from_sorted_list xs_sorted in
  binary_search_tree_repeated_lookup xs2 tree
