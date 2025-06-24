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

(* Merge sort *)

let rec merge (xs : int list) (ys : int list) current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (ys, current_original_counters)
  | x :: xs_tl -> (
      match ys with
      | [] -> (xs, current_original_counters)
      | y :: ys_tl ->
          if x <= y then
            let recursive_result, current_original_counters =
              merge xs_tl ys current_original_counters
            in
            (x :: recursive_result, current_original_counters)
          else
            let recursive_result, current_original_counters =
              merge xs ys_tl current_original_counters
            in
            (y :: recursive_result, current_original_counters))

let rec split xs current_original_counters =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> (([], []), current_original_counters)
  | [ x ] -> (([ x ], []), current_original_counters)
  | x1 :: x2 :: tl ->
      let (lower, upper), current_original_counters =
        split tl current_original_counters
      in
      ((x1 :: lower, x2 :: upper), current_original_counters)

let rec merge_sort xs current_original_counters =
  let _ = Raml.tick 1.0 in
  let new_counter = decrement_counter current_original_counters in
  let result, counter_final =
    match xs with
    | [] -> ([], new_counter)
    | [ x ] -> ([ x ], new_counter)
    | _ ->
        let (lower, upper), counter_split = split xs new_counter in
        let lower_sorted, counter1 = merge_sort lower counter_split in
        let upper_sorted, counter2 = merge_sort upper counter1 in
        merge lower_sorted upper_sorted counter2
  in
  (result, increment_counter counter_final)

(* Polynomial degree for AARA: 2 *)

let main xs current_original_counters =
  let initialized_counter = initialize_counter current_original_counters in
  let result, counter_after_sorting = merge_sort xs initialized_counter in
  (result, set_counter_to_zero counter_after_sorting)
