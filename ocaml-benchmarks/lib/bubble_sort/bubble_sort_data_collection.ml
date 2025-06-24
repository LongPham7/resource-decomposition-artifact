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

let rec bubble_sort xs =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    let is_xs_sorted, xs_swapped = traverse_and_swap xs in
    if is_xs_sorted then xs_swapped else bubble_sort xs_swapped
  in
  let _ = Raml.mark 0 (-1.0) in
  result

let main xs =
  let _ = Raml.activate_counter_variable 0 in
  let result = bubble_sort xs in
  let _ = Raml.record_counter_variable 0 in
  result
