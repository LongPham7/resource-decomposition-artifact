exception Invalid_input

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

let rec heapify_build heap (index : int) =
  let _ = Raml.mark 0 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
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
    if smallest_index_right = index then ()
    else
      let element_at_index = Rarray.get array (Rnat.of_int index) in
      let element_at_smallest_index =
        Rarray.get array (Rnat.of_int smallest_index_right)
      in
      let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
      let _ =
        Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
      in
      heapify_build heap smallest_index_right
  in
  let _ = Raml.mark 0 (-1.0) in
  result

let rec repeatedly_heapify heap index =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index
    (fun () -> ())
    (fun index_minus_one ->
      let _ = Raml.activate_counter_variable 0 in
      let _ = heapify_build heap (Rnat.to_int index_minus_one) in
      let _ = Raml.record_counter_variable 0 in
      repeatedly_heapify heap index_minus_one)

let build_min_heap heap =
  let _ = Raml.tick 1.0 in
  let _, length = heap in
  repeatedly_heapify heap length

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

let create_heap_from_list xs =
  let _ = Raml.tick 1.0 in
  let nat_length = list_nat_length xs in
  let array = Rarray.make nat_length (LeafCode (-1, -1)) in
  let _ = copy_list_to_array xs array 0 in
  let _ = build_min_heap (array, nat_length) in
  (array, nat_length)

let get_min heap =
  let _ = Raml.tick 1.0 in
  let array, _ = heap in
  Rarray.get array Rnat.zero

let rec heapify_extract heap (index : int) =
  let _ = Raml.mark 1 1.0 in
  let _ = Raml.tick 1.0 in
  let result =
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
    if smallest_index_right = index then ()
    else
      let element_at_index = Rarray.get array (Rnat.of_int index) in
      let element_at_smallest_index =
        Rarray.get array (Rnat.of_int smallest_index_right)
      in
      let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
      let _ =
        Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
      in
      heapify_extract heap smallest_index_right
  in
  let _ = Raml.mark 1 (-1.0) in
  result

let heapify_index_zero heap =
  let _ = Raml.activate_counter_variable 1 in
  let result = heapify_extract heap 0 in
  let _ = Raml.record_counter_variable 1 in
  result

let rec recursively_construct_huffman_code heap =
  let _ = Raml.tick 1.0 in
  let array, length = heap in
  Rnat.ifz length
    (fun () -> raise Invalid_input)
    (fun length_minus_one ->
      let v1 = get_min heap in
      Rnat.ifz length_minus_one
        (fun () -> v1)
        (fun _ ->
          let last_element = Rarray.get array length_minus_one in
          let _ = Rarray.set array Rnat.zero last_element in
          let heap1 = (array, length_minus_one) in
          let _ = heapify_index_zero heap1 in
          let v2 = get_min heap1 in
          let merged_code_tree = merge_code_trees v1 v2 in
          let _ = Rarray.set array Rnat.zero merged_code_tree in
          let _ = heapify_index_zero heap1 in
          recursively_construct_huffman_code heap1))

let huffman_code xs =
  let _ = Raml.tick 1.0 in
  let heap = create_heap_from_list xs in
  recursively_construct_huffman_code heap
