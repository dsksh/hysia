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

let loc_of_name id (lid,_,_,_) = id = lid

let find_inv_frontier_ lid iid _inv = 
(*Printf.printf "fif %s %d\n%!" lid iid;*)
    iid, (find_inv_frontier lid iid)

let select_earliest earliest (eid,(l,u)) = match earliest with
  | Some (eid1,(l1,u1)) -> 
(*Printf.printf "%d,[%f,%f] vs. %d,[%f,%f]\n%!" eid1 l1 u1 eid l u;*)

      if l<=u then begin
        if u < l1 then Some (eid,(l,u)) else begin
          if u1 < l then Some (eid1,(l1,u1)) else 
            error (SelectEarliestError ((l,u), (l1,u1))) end 
      end else if l = -1. then
        error (SelectEarliestError ((l,u), (l1,u1)))
      else
        Some (eid1,(l1,u1))
  | None -> 
      if l<=u then Some (eid,(l,u)) else None

let find_first_zero_ lid eid (gh,_,_dst,_) = 
    eid, (find_first_zero false lid eid)

let filter_invariant lid invs es tmax (eid,(l,u)) =
    match tmax with
    | Some (iid,(lm,um)) -> 
        (*Printf.printf "%b\n" (fst (List.nth invs iid) = gh);*)
        let (gh,_,_,_) = List.nth es eid in
        if (fst (List.nth invs iid) = gh) then true
        else begin
            if u < lm then true 
            else 
                (*error (SelectEarliestError ((l,u), (lm,um)))*)
                false
        end
    | None -> true


let select_random dst_list = 
    (*let filter dl (eid,(l,u)) = 
        if l<=u then (eid,(l,u))::dl else dl*)
    let filter (eid,(l,u)) = l<=u
    in
    let dl = List.filter filter dst_list in
    match dl with
    | [] -> None
    | dl -> Some (List.nth dl (Random.int (List.length dl)))

(*let simulate_edge id earliest (lid,_,es) =
  if id = lid then
    let (dst,(l,u)) = List.map (find_first_zero_ lid) es in
    match earliest with
    | Some (dst1,(l1,u1)) -> 
Printf.printf "%s,[%f,%f] vs. %s,[%f,%f]\n%!" dst1 l1 u1 dst l u;

      if l<=u then begin
        if u < l1 then Some (dst,(l,u)) else begin
          if u1 < l then Some (dst1,(l1,u1)) else 
            error (SelectEarliestError ((l,u), (l1,u1))) end end
      else Some (dst1,(l1,u1))
    | None -> 
      if l<=u then Some (dst,(l,u)) else None
      (*List.fold_left select_earliest dst zs*)
  else dst
*)

let dst_of_edge (_,_,dst,_) = dst

let simulate (_ps,_var,(iloc,_ival),flocs,locs) =
  initialize ();
  let curr_loc = ref iloc in
  print_pped true false;
  for i = 1 to (if !step_max >= 0 then !step_max else max_int) do
    (*Printf.printf "step %d at %s\n%!" i !curr_loc;*)
    report_step i !curr_loc;

    (* TODO: check the loc invariant *)

    (* compute the earliest time reaching the inv frontier. *)
    let (_,_,invs,_) = List.find (loc_of_name !curr_loc) locs in
    let fs = List.mapi (find_inv_frontier_ !curr_loc) invs in
(*Printf.printf "fif done\n%!";*)
    let tmax = List.fold_left select_earliest None fs in

    (* find zero for each edge *)
    let (_,_,_,es) = List.find (loc_of_name !curr_loc) locs in
    let zs = List.mapi (find_first_zero_ !curr_loc) es in
    let zs = List.filter (filter_invariant !curr_loc invs es tmax) zs in
    (*let dst = List.fold_left select_earliest None zs in*)
    let dst = select_random zs in

    match dst with
    | Some (eid,(l0,u0)) ->
      (* FIXME *)
      find_first_zero true !curr_loc eid;
      if find_first_zero_mid !curr_loc eid then begin
        simulate_jump !curr_loc eid l0 u0;
        print_pped false false;
        curr_loc := dst_of_edge (List.nth es eid)
      end else error FindZeroMidError;

      begin try 
          List.find (fun lid -> lid = !curr_loc) flocs;
          print_endline "reached final loc!"
        with Not_found -> ()
      end

    | None ->
      simulate_cont !curr_loc;
      dispose ();
      error FindZeroError
  done;
  print_pped true true;
  dispose ()
