exception Invalid_input

type avl_tree = Leaf | Node of int * int * avl_tree * avl_tree

let rec avl_tree_lookup v tree =
  let _ = Raml.mark 1 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match tree with
    | Leaf -> false
    | Node (x, _, l, r) ->
        if x = v then true
        else if v < x then avl_tree_lookup v l
        else avl_tree_lookup v r
  in
  let _ = Raml.mark 1 (-1.0) in
  result

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

let rec avl_tree_insert v tree =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match tree with
    | Node (x, _, l, r) ->
        if x = v then tree
        else if v < x then
          let insL = avl_tree_insert v l in
          let dl = depth insL in
          let dr = depth r in
          let bal = dl - dr in
          if bal < 2 || bal > 2 then Node (x, max dr dl + 1, insL, r)
          else if v < value l then balanceLL (Node (x, dl + 1, insL, r))
          else if v > value l then balanceLR (Node (x, dl + 1, insL, r))
          else tree
        else
          let insR = avl_tree_insert v r in
          let dr = depth insR in
          let dl = depth l in
          let bal = dl - dr in
          if bal < -2 || bal > -2 then Node (x, max dr dl + 1, l, insR)
          else if v > value r then balanceRR (Node (x, dr + 1, l, insR))
          else if v < value r then balanceRL (Node (x, dr + 1, l, insR))
          else tree
    | Leaf -> Node (v, 1, Leaf, Leaf)
  in
  let _ = Raml.mark 0 (-1.0) in
  result

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

let rec avl_tree_repeated_insert xs acc =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> acc
  | hd :: tl ->
      let _ = Raml.activate_counter_variable 0 in
      let acc_updated = avl_tree_insert hd acc in
      let _ = Raml.record_counter_variable 0 in
      avl_tree_repeated_insert tl acc_updated

let rec avl_tree_repeated_lookup xs tree =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> []
  | hd :: tl ->
      let _ = Raml.activate_counter_variable 1 in
      let is_found = avl_tree_lookup hd tree in
      let _ = Raml.record_counter_variable 1 in
      let recursive_result = avl_tree_repeated_lookup tl tree in
      is_found :: recursive_result

let avl_tree_main xs1 xs2 =
  let _ = Raml.tick 1.0 in
  let tree = avl_tree_repeated_insert xs1 Leaf in
  avl_tree_repeated_lookup xs2 tree
