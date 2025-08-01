let rec partition (pivot : int) (xs : int list) =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ([], [])
  | hd :: tl ->
      let lower, upper = partition pivot tl in
      if hd < pivot then (hd :: lower, upper) else (lower, hd :: upper)

let rec append xs ys =
  let _ = Raml.tick 1.0 in
  match xs with [] -> ys | hd :: tl -> hd :: append tl ys

let rec quicksort xs =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match xs with
    | [] -> []
    | [ x ] -> [ x ]
    | hd :: tl ->
        let lower, upper = partition hd tl in
        let lower_sorted = quicksort lower in
        let upper_sorted = quicksort upper in
        append lower_sorted (hd :: upper_sorted)
  in
  let _ = Raml.mark 0 (-1.0) in
  result

let main xs =
  let _ = Raml.activate_counter_variable 0 in
  let result = quicksort xs in
  let _ = Raml.record_counter_variable 0 in
  result
