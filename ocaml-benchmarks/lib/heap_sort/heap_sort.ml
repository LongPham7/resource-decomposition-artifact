exception Invalid_input

(* The type binary_heap implements an array-based binary min-heap. The type has
   two components: (i) the array storing all elements and (ii) the current size
   of the heap. *)

type binary_heap = int Rarray.t * Rnat.t

let rec heapify (heap : int Rarray.t * Rnat.t) (index : int) =
  let _ = Raml.tick 1.0 in
  let left_index = (index * 2) + 1 and right_index = (index * 2) + 2 in
  let array, length = heap in
  (* Figure out which of the current element (i.e., element at index) and the
     left child has a higher priority *)
  let smallest_index_left =
    if left_index < Rnat.to_int length then
      let element_index = Rarray.get array (Rnat.of_int index) in
      let element_left_index = Rarray.get array (Rnat.of_int left_index) in
      if element_left_index < element_index then left_index else index
    else index
  in
  (* Figure out which of the element at the index smallest_index_left and the
     right child has a higher priority *)
  let smallest_index_right =
    if right_index < Rnat.to_int length then
      let element_smallest_index_left =
        Rarray.get array (Rnat.of_int smallest_index_left)
      in
      let element_right_index = Rarray.get array (Rnat.of_int right_index) in
      if element_right_index < element_smallest_index_left then right_index
      else smallest_index_left
    else smallest_index_left
  in
  if smallest_index_right = index then ()
  else
    let element_at_index = Rarray.get array (Rnat.of_int index) in
    let element_at_smallest_index =
      Rarray.get array (Rnat.of_int smallest_index_right)
    in
    (* Swap the two elements: (i) the element at index and (ii) the element
       at the index smallest_index_right *)
    let _ = Rarray.set array (Rnat.of_int index) element_at_smallest_index in
    let _ =
      Rarray.set array (Rnat.of_int smallest_index_right) element_at_index
    in
    heapify heap smallest_index_right

(* Given a heap where the min-heap property is not necessarily satisfied,
   successively heapify the heap, in the order from the last element to the
   first element *)

let rec repeatedly_heapify heap index =
  let _ = Raml.tick 1.0 in
  Rnat.ifz index
    (fun () -> ())
    (fun index_minus_one ->
      let _ = heapify heap (Rnat.to_int index_minus_one) in
      repeatedly_heapify heap index_minus_one)

let build_min_heap heap =
  let _ = Raml.tick 1.0 in
  let _, length = heap in
  repeatedly_heapify heap length

let rec copy_list_to_array xs array index =
  let _ = Raml.tick 1.0 in
  match xs with
  | [] -> ()
  | hd :: tl ->
      let _ = Rarray.set array (Rnat.of_int index) hd in
      copy_list_to_array tl array (index + 1)

let rec list_nat_length xs =
  let _ = Raml.tick 1.0 in
  match xs with [] -> Rnat.zero | _ :: tl -> Rnat.succ (list_nat_length tl)

(* Create a binary min-heap from a list *)

let create_heap_from_list xs =
  let _ = Raml.tick 1.0 in
  let nat_length = list_nat_length xs in
  let array = Rarray.make nat_length 0 in
  let _ = copy_list_to_array xs array 0 in
  let _ = build_min_heap (array, nat_length) in
  (array, nat_length)

(* Repeatedly extract the minimum element from the heap *)

let rec extract_list_from_heap heap =
  let _ = Raml.tick 1.0 in
  let array, length = heap in
  Rnat.ifz length
    (fun () -> [])
    (fun length_minus_one ->
      let min_element = Rarray.get array Rnat.zero in
      let last_element = Rarray.get array length_minus_one in
      (* Move the last element to the root of the heap (i.e., the first cell
         in the array) *)
      let _ = Rarray.set array Rnat.zero last_element in
      let _ = heapify heap 0 in
      let recursive_result = extract_list_from_heap (array, length_minus_one) in
      min_element :: recursive_result)

(* Polynomial degree for AARA: N/A *)

let heap_sort xs =
  let _ = Raml.tick 1.0 in
  let heap = create_heap_from_list xs in
  extract_list_from_heap heap
