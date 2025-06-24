(* This OCaml implementation of the red-black tree comes from this web page:
   https://cs3110.github.io/textbook/chapters/ds/rb.html. *)

exception Invalid_input

type color = Red | Black

type red_black_tree =
  | Leaf
  | Node of color * int * red_black_tree * red_black_tree

let rec red_black_tree_lookup v tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> false
  | Node (_, x, left, right) ->
      if x = v then true
      else if v < x then red_black_tree_lookup v left
      else red_black_tree_lookup v right

let balance color v t1 t2 =
  let _ = Raml.tick 1.0 in
  match (color, v, t1, t2) with
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d)

let rec red_black_tree_insert_helper x tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> Node (Red, x, Leaf, Leaf)
  | Node (color, y, a, b) ->
      if x < y then balance color y (red_black_tree_insert_helper x a) b
      else if x > y then balance color y a (red_black_tree_insert_helper x b)
      else tree

let red_black_tree_insert x tree =
  let _ = Raml.tick 1.0 in
  let insert_result = red_black_tree_insert_helper x tree in
  match insert_result with
  | Node (_, y, a, b) -> Node (Black, y, a, b)
  | Leaf ->
      (* guaranteed to be non-empty *)
      raise Invalid_input

(* Successively insert elements into a red-black tree *)

let rec red_black_tree_repeated_insert xs acc =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> acc
  | hd :: tl ->
      let acc_updated = red_black_tree_insert hd acc in
      red_black_tree_repeated_insert tl acc_updated

let rec red_black_tree_repeated_lookup xs tree =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> []
  | hd :: tl ->
      let is_found = red_black_tree_lookup hd tree in
      let recursive_result = red_black_tree_repeated_lookup tl tree in
      is_found :: recursive_result

(* We first build a red-black tree by successively inserting the elements of the
   first input list xs1 to the tree. We then successively look up the elements
   of the second input list xs2 in the tree. *)

(* Polynomial degree for AARA: 2 *)

let red_black_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let tree = red_black_tree_repeated_insert xs1 Leaf in
  red_black_tree_repeated_lookup xs2 tree
