open Model_common
open Capd_simulating_stubs
open Util

(*type interval = float * float*)

let step_max = ref 5

(*let integrate args =
  let a1 = try MParam.find "t_end" args with Not_found -> 1. in
  let a2 = try MParam.find "order" args with Not_found -> 10. in
  let a3 = try MParam.find "h_min" args with Not_found -> 0.1 in
  let a4 = try MParam.find "h_max" args with Not_found -> 1. in
  integrate a1 a2 a3 a4
*)

(*let simulate _ =
  initialize ();
  print_pped true false;
  for i = 1 to !step_max do
    Printf.printf "step %d\n%!" i;
    if find_first_zero "L" then begin
      if find_first_zero_mid "L" then begin
        (*print_pped true false;*)
        simulate_jump "L";
        print_pped false false 
  
      end else error FindZeroMidError
    end else error FindZeroError
  done;
  print_pped true true;
  dispose ()
*)

let loc_of_name id (lid,_,_) = id = lid

let find_first_zero_ lid (_,_,dst,_) = dst, (find_first_zero false lid dst)

let select_earliest earliest (dst,(l,u)) = match earliest, (l,u) with
  | Some (dst1,(l1,u1)), (l,u) -> 
      if l<=u then begin
        if u < l1 then Some (dst,(l,u)) else begin
          if u1 < l then Some (dst1,(l1,u1)) else 
            error (SelectEarliestError ((l,u), (l1,u1))) end end
      else Some (dst1,(l1,u1))
  | None, (l,u) -> 
      if l<=u then Some (dst,(l,u)) else None

let simulate (_ps,_var,(iloc,_ival),locs) =
  initialize ();
  let lid = ref iloc in
  print_pped true false;
  for i = 1 to !step_max do
    (*Printf.printf "step %d at %s\n%!" i !lid;*)
    report_step i !lid;
    let (_,_,es) = List.find (loc_of_name !lid) locs in
    let zs = List.map (find_first_zero_ !lid) es in
    let dst = List.fold_left select_earliest None zs in
    match dst with
    | Some (dst,(l0,u0)) ->
      (* FIXME *)
      find_first_zero true !lid dst;
      if find_first_zero_mid !lid dst then begin
        simulate_jump !lid dst l0 u0;
        print_pped false false;
        lid := dst
      end else error FindZeroMidError
    | None ->
      error FindZeroError
  done;
  print_pped true true;
  dispose ()
