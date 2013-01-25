open Model_common
open Capd_simulating_stubs
open Util

let step_max= ref 5

(*let integrate args =
  let a1 = try MParam.find "t_end" args with Not_found -> 1. in
  let a2 = try MParam.find "order" args with Not_found -> 10. in
  let a3 = try MParam.find "h_min" args with Not_found -> 0.1 in
  let a4 = try MParam.find "h_max" args with Not_found -> 1. in
  integrate a1 a2 a3 a4
*)

let simulate () =
  initialize ();
  for i = 1 to !step_max do
    Printf.printf "step %d\n" i;
    if find_first_zero () then
      if find_first_zero_mid () then
        simulate_jump ()
  
      else error FindZeroMidError
    else error FindZeroError
  done
