open Core

type 'a pList = PList of 'a list * 'a list

(* Convert natural numbers to bit vectors. Note that the input must be a natural
   number (i.e., a non-negative integer). *)

let convert_int_to_bit_vector x =
  (* let _ = Raml.tick 1.0 in *)
  let rec convert_int_acc x acc =
    if x = 0 then acc
    else if x = 1 then 1 :: acc
    else convert_int_acc (x / 2) ((x mod 2) :: acc)
  in
  convert_int_acc x []

(* Compare two bit vectors representing natural numbers. The result of
   comparison between x and y is (i) -1 if x < y; (ii) 0 if x = y; and (iii) 1
   if x > y. The cost of the function compare_bit_vectors_maximum_length is
   given by its number of function calls. Hence, we disregard the cost of
   calling helper functions: convert_int_to_bit_vector and List.rev. *)

let compare_bit_vectors_maximum_length xs ys =
  (* The helper function compares two reversed bit vectors (i.e., the least
     significant digit comes first). The variable acc stores the result of
     comparing prefixes of the reversed bit vectors so far. *)
  let rec compare_bits_helper xs ys acc =
    let _ = Raml.tick 1.0 in
    match (xs, ys) with
    | [], [] -> acc
    | [], _ :: tl -> compare_bits_helper [] tl (-1)
    | _ :: tl, [] -> compare_bits_helper tl [] 1
    | hd1 :: tl1, hd2 :: tl2 ->
        if hd1 = hd2 then compare_bits_helper tl1 tl2 acc
        else if hd1 < hd2 then compare_bits_helper tl1 tl2 (-1)
        else compare_bits_helper tl1 tl2 1
  in
  let xs_reversed = List.rev xs in
  let ys_reversed = List.rev ys in
  compare_bits_helper xs_reversed ys_reversed 0

(* Compare two natural numbers by first converting them to bit vectors and then
   traversing them. This comparison function only works for natural numbers
   (i.e., non-negative integers). *)

let le_complex (x, y) =
  let _ = Raml.tick 1.0 in
  let x_bit_vector = convert_int_to_bit_vector x in
  let y_bit_vector = convert_int_to_bit_vector y in
  let comparison_result =
    compare_bit_vectors_maximum_length x_bit_vector y_bit_vector
  in
  if comparison_result < 1 then true else false

let rec partition (xs, pivot) =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> PList ([], [])
  | hd :: tl -> (
      match partition (tl, pivot) with
      | PList (left, right) ->
          if le_complex (hd, pivot) then PList (hd :: left, right)
          else PList (left, hd :: right))

let rec append (xs, ys) =
  let _ = Raml.tick 1.0 in
  match xs with [] -> ys | hd :: tl -> hd :: append (tl, ys)

let rec quicksort xs =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> []
  | hd :: tl -> (
      match partition (tl, hd) with
      | PList (left, right) ->
          let left_sorted = quicksort left in
          let right_sorted = quicksort right in
          append (left_sorted, hd :: right_sorted))
