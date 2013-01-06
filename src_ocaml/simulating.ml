open Model_common
open Capd_simulating_stubs

let integrate args =
  let a1 = try MParam.find "t_end" args with Not_found -> 1. in
  let a2 = try MParam.find "order" args with Not_found -> 10. in
  let a3 = try MParam.find "h_min" args with Not_found -> 0.1 in
  let a4 = try MParam.find "h_max" args with Not_found -> 1. in
  integrate a1 a2 a3 a4

let simulate () =
  initialize ();
  find_zero ()
