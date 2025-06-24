(* This OCaml implementation of the splay tree comes from this web page:
   https://github.com/aduraj/ocaml-data-structures/blob/master/Splay/splaytree.ml.
*)

exception Invalid_input

type splay_tree = Leaf | Node of int * splay_tree * splay_tree

let extract_value_and_subtrees tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> raise Invalid_input
  | Node (x, left, right) -> (x, left, right)

let rec splay x tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> Leaf
  | Node (y, l, r) -> (
      if y = x then tree
      else if x < y then
        match l with
        | Leaf -> tree
        | Node (z, ll, rr) -> (
            if x = z then Node (z, ll, Node (y, rr, r))
            else if x < z then
              match ll with
              | Leaf -> Node (z, ll, Node (y, rr, r))
              | _ ->
                  let newV, newL, newR =
                    extract_value_and_subtrees (splay x ll)
                  in
                  Node (newV, newL, Node (z, newR, Node (y, rr, r)))
            else
              match rr with
              | Leaf -> Node (z, ll, Node (y, rr, r))
              | _ ->
                  let newV, newL, newR =
                    extract_value_and_subtrees (splay x rr)
                  in
                  Node (newV, Node (z, ll, newL), Node (y, newR, r)) )
      else
        match r with
        | Leaf -> tree
        | Node (z, ll, rr) -> (
            if x = z then Node (z, Node (y, l, ll), rr)
            else if x < z then
              match ll with
              | Leaf -> Node (z, Node (y, l, ll), rr)
              | _ ->
                  let newV, newL, newR =
                    extract_value_and_subtrees (splay x ll)
                  in
                  Node (newV, Node (y, l, newL), Node (z, newR, rr))
            else
              match rr with
              | Leaf -> Node (z, Node (y, l, ll), rr)
              | _ ->
                  let newV, newL, newR =
                    extract_value_and_subtrees (splay x rr)
                  in
                  Node (newV, Node (z, Node (y, l, ll), newL), newR) ) )

let splay_tree_insert x tree =
  let _ = Raml.tick 1.0 in
  match tree with
  | Leaf -> Node (x, Leaf, Leaf)
  | _ ->
      let tree_splayed = splay x tree in
      let y, l, r = extract_value_and_subtrees tree_splayed in
      if x = y then Node (y, l, r)
      else if x < y then Node (x, l, Node (y, Leaf, r))
      else Node (x, Node (y, l, Leaf), r)

let splay_tree_lookup v tree =
  let _ = Raml.tick 1.0 in
  let tree_splayed = splay v tree in
  let is_found =
    match tree_splayed with
    | Leaf -> false
    | Node (x, _, _) -> if x = v then true else false
  in
  (is_found, tree_splayed)

(* Successively insert elements into a splay tree *)

let rec splay_tree_repeated_insert xs acc =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> acc
  | hd :: tl ->
      let acc_updated = splay_tree_insert hd acc in
      splay_tree_repeated_insert tl acc_updated

let rec splay_tree_repeated_lookup xs tree =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], tree)
  | hd :: tl ->
      let is_found, tree_updated = splay_tree_lookup hd tree in
      let recursive_result, tree_final =
        splay_tree_repeated_lookup tl tree_updated
      in
      (is_found :: recursive_result, tree_final)

(* We first build a splay tree by successively inserting the elements of the
   first input list xs1 to the tree. We then successively look up the elements
   of the second input list xs2 in the tree. *)

(* Polynomial degree for AARA: 2 *)

let splay_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let tree = splay_tree_repeated_insert xs1 Leaf in
  splay_tree_repeated_lookup xs2 tree
