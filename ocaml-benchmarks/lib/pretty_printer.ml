open Core

let print_out_integer_list list =
  List.iter list ~f:(fun x -> printf "%d " x);
  print_endline ""

let print_out_total_cost () =
  let total_cost = Raml.get_total_cost () in
  printf "Total cost: %f\n" total_cost

let print_out_recorded_counter_values () =
  let list_counter_values = Raml.get_counter_values () in
  printf "Recorded counter values: ";
  List.iter list_counter_values ~f:(fun (index, x) ->
      printf "(%d, %f) " index x);
  print_endline ""
