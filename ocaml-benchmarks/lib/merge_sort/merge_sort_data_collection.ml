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
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
    match xs with
    | [] -> []
    | [ x ] -> [ x ]
    | _ ->
        let lower, upper = split xs in
        let lower_sorted = merge_sort lower in
        let upper_sorted = merge_sort upper in
        merge lower_sorted upper_sorted
  in
  let _ = Raml.mark 0 (-1.0) in
  result

let main xs =
  let _ = Raml.activate_counter_variable 0 in
  let result = merge_sort xs in
  let _ = Raml.record_counter_variable 0 in
  result
