open Core

(* This module creates (i) a reference total_cost for counting the total
   computational cost and (ii) a reference total_mark for collecting runtime
   data for a counter variable. *)

(* Total cost counter. To increment the total cost by q, we write Raml.tick in
   to the source code of a benchmark function, where Raml is the name of this
   module. Conveniently, the syntax Raml.tick q is also used in RaML to indicate
   the consumption of q units of resources. *)

let total_cost = ref 0.0
let set_total_cost_to_zero () = total_cost := 0.0
let tick q = total_cost := !total_cost +. q
let get_total_cost () = !total_cost

(* Counter variable (which is a vector containing possibly multiple counters).
   The counter variable is implemented by a reference to an array, whose length
   is equal to the number of constituent counters.

   Each counter in the array contains three components: (i) a Boolean flag
   indicating whether the counter variable is active or inactive, (ii) a
   high-water-mark cost, and (iii) the net cost. A counter is activated when we
   enter an annotated code fragment for a counter (indicated by Raml.counter(f
   x)). Once we exit this code fragment, the counter is deactivated. If we try
   to increment an inactive counter, we raise an exception. *)

let counter_variable = ref [||]
let recorded_counter_values : (int * float) list ref = ref []

let initialize_counter_variable num_counters =
  let _ =
    counter_variable := Array.create ~len:num_counters (false, 0.0, 0.0)
  in
  recorded_counter_values := []

let activate_counter_variable index =
  !counter_variable.(index) <- (true, 0.0, 0.0)

let mark index q =
  let is_active, high_water_mark, net_cost = !counter_variable.(index) in
  if not is_active then
    failwith "We cannot increment an inactive counter variable"
  else
    let net_cost_updated = net_cost +. q in
    let high_water_mark_updated = Float.max high_water_mark net_cost_updated in
    !counter_variable.(index) <-
      (true, high_water_mark_updated, net_cost_updated)

let record_counter_variable index =
  let is_active, high_water_mark, _ = !counter_variable.(index) in
  if not is_active then failwith "We cannot record an inactive counter variable"
  else
    let _ =
      recorded_counter_values :=
        (index, high_water_mark) :: !recorded_counter_values
    in
    !counter_variable.(index) <- (false, 0.0, 0.0)

let get_counter_values () = !recorded_counter_values
