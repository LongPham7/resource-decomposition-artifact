exception Invalid_input

(* Counter-related functions *)

let decrement_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  match current_counter with
  | [] -> raise Invalid_input
  | _ :: counter_tl -> (counter_tl, original_counter)

let increment_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  (1 :: current_counter, original_counter)

let initialize_counter current_original_counters =
  let _, original_counter = current_original_counters in
  (original_counter, original_counter)

let set_counter_to_zero current_original_counters =
  let _, original_counter = current_original_counters in
  ([], original_counter)

(* Bubble sort *)

let rec traverse_and_swap (xs : int list) current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] | [ _ ] -> ((true, xs), current_original_counters)
  | x1 :: x2 :: tl ->
      if x1 <= x2 then
        let (is_tl_sorted, tl_swapped), current_original_counters =
          traverse_and_swap (x2 :: tl) current_original_counters
        in
        ((is_tl_sorted, x1 :: tl_swapped), current_original_counters)
      else
        let (_, tl_swapped), current_original_counters =
          traverse_and_swap (x1 :: tl) current_original_counters
        in
        ((false, x2 :: tl_swapped), current_original_counters)

let rec bubble_sort xs current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_counter current_original_counters in
  let (is_xs_sorted, xs_swapped), counter_after_swap =
    traverse_and_swap xs new_counter
  in
  let result, counter_final =
    if is_xs_sorted then (xs_swapped, counter_after_swap)
    else bubble_sort xs_swapped counter_after_swap
  in
  (result, increment_counter counter_final)

(* Polynomial degree for AARA: 2 *)

let main xs current_original_counters =
  let initialized_counter = initialize_counter current_original_counters in
  let result, counter_after_sorting = bubble_sort xs initialized_counter in
  (result, set_counter_to_zero counter_after_sorting)
