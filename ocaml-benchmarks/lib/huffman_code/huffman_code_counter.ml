exception Invalid_input

(* Counter-related functions. We have two counters: (i) the first counter for
   tracking the recursion depth of the function heapify inside the function
   repeatedly_heapify and (ii) the second counter for counting the recursion
   depth of the function heapify inside the function heapify_index_zero. *)

let decrement_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  match current_counter with
  | [] -> raise Invalid_input
  | _ :: counter_tl -> (counter_tl, original_counter)

let decrement_first_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (decrement_counter current_original_counters1, current_original_counters2)

let decrement_second_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, decrement_counter current_original_counters2)

let increment_counter current_original_counters =
  let current_counter, original_counter = current_original_counters in
  (1 :: current_counter, original_counter)

let increment_first_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (increment_counter current_original_counters1, current_original_counters2)

let increment_second_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, increment_counter current_original_counters2)

let initialize_counter current_original_counters =
  let _, original_counter = current_original_counters in
  (original_counter, original_counter)

let initialize_first_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (initialize_counter current_original_counters1, current_original_counters2)

let initialize_second_counter two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, initialize_counter current_original_counters2)

let set_counter_to_zero current_original_counters =
  let _, original_counter = current_original_counters in
  ([], original_counter)

let set_first_counter_to_zero two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (set_counter_to_zero current_original_counters1, current_original_counters2)

let set_second_counter_to_zero two_current_original_counters =
  let current_original_counters1, current_original_counters2 =
    two_current_original_counters
  in
  (current_original_counters1, set_counter_to_zero current_original_counters2)

(* Huffman code *)

type code_tree =
  | LeafCode of int * int
  | NodeCode of int * code_tree * code_tree

let code_tree_count v =
  let _ = Raml.tick 1.0 in
  match v with LeafCode (_, count) -> count | NodeCode (count, _, _) -> count

let merge_code_trees v1 v2 =
  let _ = Raml.tick 1.0 in
  let count1, count2 = (code_tree_count v1, code_tree_count v2) in
  NodeCode (count1 + count2, v1, v2)

type binary_heap = int Rarray.t * Rnat.t

let rec heapify_build heap (index : int) two_current_original_counters =
  let new_counter = decrement_first_counter two_current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
    let left_index = (index * 2) + 1 and right_index = (index * 2) + 2 in
    let array, length = heap in
    let smallest_index_left =
      if left_index < Rnat.to_int length then
        let element_index = Rarray.get array (Rnat.of_int index) in
        let element_left_index = Rarray.get array (Rnat.of_int left_index) in
        if code_tree_count element_left_index < code_tree_count element_index
        then left_index
        else index
      else index
    in
    let smallest_index_right =
      if right_index < Rnat.to_int length then
        let element_smallest_index_left =
          Rarray.get array (Rnat.of_int smallest_index_left)
        in
        let element_right_index = Rarray.get array (Rnat.of_int right_index) in
        if
          code_tree_count element_right_index
          < code_tree_count element_smallest_index_left
        then right_index
        else smallest_index_left
      else smallest_index_left
    in
    if smallest_index_right = index then ((), new_counter)
    else
      let element_at_index = Rarray.get array (Rnat.of_int index) in
      let element_at_smallest_index =
        Rarray.get array (Rnat.of_int smallest_index_right)
      in
      let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
      let _ =
        Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
      in
      heapify_build heap smallest_index_right new_counter
  in
  (result, increment_first_counter counter_final)

let rec repeatedly_heapify heap index two_current_original_counters =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index
    (fun () -> ((), two_current_original_counters))
    (fun index_minus_one ->
      let initialized_counter =
        initialize_first_counter two_current_original_counters
      in
      let _, counter1 =
        heapify_build heap (Rnat.to_int index_minus_one) initialized_counter
      in
      let counter2 = set_first_counter_to_zero counter1 in
      repeatedly_heapify heap index_minus_one counter2)

let build_min_heap heap two_current_original_counters =
  let _ = Raml.tick 1.0 in
  let _, length = heap in
  repeatedly_heapify heap length two_current_original_counters

let rec copy_list_to_array xs array index =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ()
  | (character, count) :: tl ->
      let _ =
        Rarray.set array (Rnat.of_int index) (LeafCode (character, count))
      in
      copy_list_to_array tl array (index + 1)

let rec list_nat_length xs =
  let _ = Raml.tick 1.0 in
  match xs with [] -> Rnat.zero | _ :: tl -> Rnat.succ (list_nat_length tl)

let create_heap_from_list xs two_current_original_counters =
  let _ = Raml.tick 1.0 in
  let nat_length = list_nat_length xs in
  let array = Rarray.make nat_length (LeafCode (-1, -1)) in
  let _ = copy_list_to_array xs array 0 in
  let _, counter1 =
    build_min_heap (array, nat_length) two_current_original_counters
  in
  ((array, nat_length), counter1)

let get_min heap =
  let _ = Raml.tick 1.0 in
  let array, _ = heap in
  Rarray.get array Rnat.zero

let rec heapify_extract heap (index : int) two_current_original_counters =
  let new_counter = decrement_second_counter two_current_original_counters in
  let result, counter_final =
    let _ = Raml.tick 1.0 in
    let left_index = (index * 2) + 1 and right_index = (index * 2) + 2 in
    let array, length = heap in
    let smallest_index_left =
      if left_index < Rnat.to_int length then
        let element_index = Rarray.get array (Rnat.of_int index) in
        let element_left_index = Rarray.get array (Rnat.of_int left_index) in
        if code_tree_count element_left_index < code_tree_count element_index
        then left_index
        else index
      else index
    in
    let smallest_index_right =
      if right_index < Rnat.to_int length then
        let element_smallest_index_left =
          Rarray.get array (Rnat.of_int smallest_index_left)
        in
        let element_right_index = Rarray.get array (Rnat.of_int right_index) in
        if
          code_tree_count element_right_index
          < code_tree_count element_smallest_index_left
        then right_index
        else smallest_index_left
      else smallest_index_left
    in
    if smallest_index_right = index then ((), new_counter)
    else
      let element_at_index = Rarray.get array (Rnat.of_int index) in
      let element_at_smallest_index =
        Rarray.get array (Rnat.of_int smallest_index_right)
      in
      let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
      let _ =
        Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
      in
      heapify_extract heap smallest_index_right new_counter
  in
  (result, increment_second_counter counter_final)

let heapify_index_zero heap two_current_original_counters =
  let initialized_counter =
    initialize_second_counter two_current_original_counters
  in
  let result, counter1 = heapify_extract heap 0 initialized_counter in
  (result, set_second_counter_to_zero counter1)

let rec recursively_construct_huffman_code heap two_current_original_counters =
  let _ = Raml.tick 1.0 in
  let array, length = heap in
  Rnat.ifz length
    (fun () -> raise Invalid_input)
    (fun length_minus_one ->
      let v1 = get_min heap in
      Rnat.ifz length_minus_one
        (fun () -> (v1, two_current_original_counters))
        (fun _ ->
          let last_element = Rarray.get array length_minus_one in
          let _ = Rarray.set array Rnat.zero last_element in
          let heap1 = (array, length_minus_one) in
          let _, counter1 =
            heapify_index_zero heap1 two_current_original_counters
          in
          let v2 = get_min heap1 in
          let merged_code_tree = merge_code_trees v1 v2 in
          let _ = Rarray.set array Rnat.zero merged_code_tree in
          let _, counter2 = heapify_index_zero heap1 counter1 in
          recursively_construct_huffman_code heap1 counter2))

(* Polynomial degree for AARA: 2 *)

let huffman_code xs two_current_original_counters =
  let _ = Raml.tick 1.0 in
  let heap, counter1 = create_heap_from_list xs two_current_original_counters in
  recursively_construct_huffman_code heap counter1
