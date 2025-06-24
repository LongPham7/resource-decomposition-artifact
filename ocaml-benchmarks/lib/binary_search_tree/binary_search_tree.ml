exception Invalid_input

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

let rec merge_sort xs =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> []
  | [ x ] -> [ x ]
  | _ ->
      let lower, upper = split xs in
      let lower_sorted = merge_sort lower in
      let upper_sorted = merge_sort upper in
      merge lower_sorted upper_sorted

(* Binary search tree *)

type binary_tree = Leaf | Node of int * binary_tree * binary_tree

(* Construct a balanced binary search tree from a sorted list in linear time *)

type nat = Zero | Succ of nat

(* The function divide_nat_by_two divides an input natural number (encoded using
   a recursive type) by two. Here, we assume that the integer division by two
   can be performed in constant time, instead of linear time, even though we
   traverse the input natural number to perform the division. Therefore, we
   disregard the cost (i.e., the number of function calls) of this function.

   Here is the rationale for this design choice. In the original
   implementation of the linear-time function
   balanced_binary_tree_from_sorted_list_helper, we use an integer for the
   input n. However, because AARA cannot assign potential to integers, it
   fails to infer a polynomial bound. To fix this issue, we create a counter
   to track the number of recursive calls, which is linear. We then use our
   custom probabilistic model for Bayesian inference to statistically infer a
   linear bound of this counter.

   However, this is not ideal, since the probabilistic model was originally
   designed for recursion depths, not other quantities (e.g., number of
   recursive calls). We could have the counter track the recursion depth of
   the function balanced_binary_tree_from_sorted_list_helper, which is
   logarithmic. However, it would yield a cost bound of the form n*log(n),
   rather than n.

   To this end, we use AARA to automatically infer the linear bound of the
   function by encoding the input n of the function
   balanced_binary_tree_from_sorted_list_helper using an algebraic data type.
   However, we need to divide the input n by two. If n is encoded as an
   integer, we can just perform the division by an arithmetic operator, which
   does not count towards the cost. However, because we now encode the input n
   as a list, we need to implement the division by defining a division
   function, and it incurs a linear cost if we count all function calls. It
   then results in a quadratic bound for the function
   balanced_binary_tree_from_sorted_list_helper. Because we still want to
   derive its linear bounds, we ignore the cost of calling the function
   divide_nat_by_two. *)

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
  let _ = Raml.tick 1.0 in
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

let rec binary_search_tree_lookup v tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> false
  | Node (x, left, right) ->
      if x = v then true
      else if v < x then binary_search_tree_lookup v left
      else binary_search_tree_lookup v right

(* Successively insert elements into a binary search tree *)

let rec binary_search_tree_repeated_insert xs acc =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> acc
  | hd :: tl ->
      let acc_updated = binary_search_tree_insert hd acc in
      binary_search_tree_repeated_insert tl acc_updated

(* Successively look up elements in a binary search tree *)

let rec binary_search_tree_repeated_lookup xs tree =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> []
  | hd :: tl ->
      let is_found = binary_search_tree_lookup hd tree in
      let recursive_result = binary_search_tree_repeated_lookup tl tree in
      is_found :: recursive_result

(* We first construct a (possibly unbalanced) binary tree search by successively
   inserting the elements of the first input list xs1 into the tree. We then
   successively look up the elements of the second input list xs1 in the tree.
*)

(* Polynomial degree for AARA: 2 *)

let unbalanced_binary_search_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let tree = binary_search_tree_repeated_insert xs1 Leaf in
  binary_search_tree_repeated_lookup xs2 tree

(* We first perform merge sort on the first input list xs1. We then use the
   resulting sorted list to build a balanced binary search tree in linear time.
   Finally, we successively look up the elements from the second input list xs2
   in the tree. *)

(* Polynomial degree for AARA: N/A *)

let balanced_binary_search_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let xs_sorted = merge_sort xs1 in
  let tree = balanced_binary_tree_from_sorted_list xs_sorted in
  binary_search_tree_repeated_lookup xs2 tree
