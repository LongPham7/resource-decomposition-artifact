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

(* Quicksort *)

let rec partition (pivot : int) (xs : int list) current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (([], []), current_original_counters)
  | hd :: tl ->
      let (lower, upper), current_original_counters =
        partition pivot tl current_original_counters
      in
      let _ = Raml.tick 1.0 in
      if hd < pivot then ((hd :: lower, upper), current_original_counters)
      else ((lower, hd :: upper), current_original_counters)

let rec append xs ys current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (ys, current_original_counters)
  | hd :: tl ->
      let result_recursive, current_original_counters =
        append tl ys current_original_counters
      in
      (hd :: result_recursive, current_original_counters)

let rec quicksort xs current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_counter current_original_counters in
  let result, counter_final =
    match xs with
    | [] -> ([], new_counter)
    | [ x ] -> ([ x ], new_counter)
    | hd :: tl ->
        let (lower, upper), counter1 = partition hd tl new_counter in
        let lower_sorted, counter2 = quicksort lower counter1 in
        let upper_sorted, counter3 = quicksort upper counter2 in
        append lower_sorted (hd :: upper_sorted) counter3
  in
  (result, increment_counter counter_final)

(* Polynomial degree for AARA: 2 *)

let main xs current_original_counters =
  let initialized_counter = initialize_counter current_original_counters in
  let result, counter_after_sorting = quicksort xs initialized_counter in
  (result, set_counter_to_zero counter_after_sorting)
