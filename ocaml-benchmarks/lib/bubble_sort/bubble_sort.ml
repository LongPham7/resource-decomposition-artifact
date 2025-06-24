let rec traverse_and_swap (xs : int list) =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] | [ _ ] -> (true, xs)
  | x1 :: x2 :: tl ->
      if x1 <= x2 then
        let is_tl_sorted, tl_swapped = traverse_and_swap (x2 :: tl) in
        (is_tl_sorted, x1 :: tl_swapped)
      else
        let _, tl_swapped = traverse_and_swap (x1 :: tl) in
        (false, x2 :: tl_swapped)

(* Polynomial degree for AARA: N/A *)

let rec bubble_sort xs =
  let _ = Raml.tick 1.0 in
  let is_xs_sorted, xs_swapped = traverse_and_swap xs in
  if is_xs_sorted then xs_swapped else bubble_sort xs_swapped
