open Model_common
open Capd_simulating_stubs
open Util

let step_max = ref max_int
let time_max = ref infinity

let loc_of_name id loc = id = Model.id_of_loc loc

let find_inv_frontier_ lid iid _inv = 
(*Printf.printf "fif %s %d\n%!" lid iid;*)
    iid, (find_inv_frontier lid iid)

let select_earliest earliest (id,(l,u)) = match earliest with
  | Some (id1,(l1,u1)) ->
(*Printf.printf "1: %d,[%f,%f] vs. %d,[%f,%f]\n%!" id1 l1 u1 id l u;*)

      if l<=u then begin
        if u < l1 then Some (id,(l,u)) else begin
          if u1 < l then Some (id1,(l1,u1)) else 
            error (SelectEarliestError ((l,u), (l1,u1))) end 
      end else if l = -1. then
        error (SelectEarliestError ((l,u), (l1,u1)))
      else
        Some (id1,(l1,u1))
  | None -> 
      if l<=u then Some (id,(l,u)) else None


let find_first_zero_ lid (eid,zsf,zs) (forced,gh,_,_dst,_) = 
    if forced then
        eid+1, (eid,find_first_zero false lid eid)::zsf, zs
    else
        eid+1, zsf, (eid,find_first_zero false lid eid)::zs

let select_earliest_grd lid invs es earliest (eid,(l,u)) = match earliest with
  | Some (iid,(l1,u1)), _None ->
(*Printf.printf "2: %d,[%f,%f] vs. %d,[%f,%f]\n%!" iid l1 u1 eid l u;*)

      let (_,gh,_,_,_) = List.nth es eid in
(*Printf.printf "%b\n" (fst (List.nth invs iid) = gh);*)
      if (fst (List.nth invs iid) = gh) then 
        None, Some (eid,(l,u))

      else begin
        if l<=u then begin
          if u < l1 then None, Some (eid,(l,u)) else begin
            if u1 < l then Some (iid,(l1,u1)), None else 
              error (SelectEarliestError ((l,u), (l1,u1))) end 
        end else if l = -1. then
          error (SelectEarliestError ((l,u), (l1,u1)))
        else
          Some (iid,(l1,u1)), None
      end

  | _None, Some (eid1,(l1,u1)) ->
(*Printf.printf "3: %d,[%f,%f] vs. %d,[%f,%f]\n%!" eid1 l1 u1 eid l u;*)
      if l<=u then begin
        if u < l1 then None, Some (eid,(l,u)) else begin
          if u1 < l then None, Some (eid1,(l1,u1)) else 
            error (SelectEarliestError ((l,u), (l1,u1))) end 
      end else if l = -1. then
        error (SelectEarliestError ((l,u), (l1,u1)))
      else
        None, Some (eid1,(l1,u1))

  | None, None -> 
      if l<=u then None, Some (eid,(l,u)) else None, None

let filter_invariant lid invs es tmax (eid,(l,u)) =
    match tmax with
    | Some (iid,(lm,um)), _None -> 
(*Printf.printf "4: %d,[%f,%f] vs. %d,[%f,%f]\n%!" iid lm um eid l u;*)
        let (_,gh,_,_,_) = List.nth es eid in
(*Printf.printf "%b\n%!" (fst (List.nth invs iid) = gh);*)
        if (fst (List.nth invs iid) = gh) then true
        else 
          l<=u && u < lm

    | _None, Some t when t = (eid,(l,u)) -> true
    | _None, Some (_,(lm,um)) -> l<=u && u < lm

    | None, None -> l<=u


let select_random dst_list = 
    (*let filter dl (eid,(l,u)) = 
        if l<=u then (eid,(l,u))::dl else dl*)
    let filter (eid,(l,u)) = l<=u
    in
    let dl = List.filter filter dst_list in
    match dl with
    | [] -> None
    | dl -> Some (List.nth dl (Random.int (List.length dl)))

let dst_of_edge (_,_,_,dst,_) = dst

let set_param_ lid (id,bnd) = 
  set_param lid id (Random.float bnd)


let find_prop_frontier_ lid t0 tmax polar (apid,tlist) =
(*Printf.printf "fpf_ %d %f %f\n%!" apid t0 tmax;*)
  let tlist = ref tlist in
  let time_l = ref t0 in
  (*let polar = ref polar in*)
  while !time_l >= 0. && !time_l < tmax do
    let (l,u) = find_prop_frontier lid apid !polar !time_l tmax in
(*Printf.printf "fpf %f %f\n%!" l u;*)
    if l > !time_l && l <= u then begin
      tlist := List.append !tlist [(Interval (l,u),!polar)];
      time_l := l;
      polar := not !polar
    end else
      time_l := -1.;
  done;
  apid, !tlist

let simulate (ps,_var,(iloc,_ival),locs) aps =
    let curr_step = ref 0 in
    let curr_loc = ref iloc in
    let curr_time_l = ref 0. in
    initialize ();
    print_pped true false;

    (* initialize ap frontiers list. *)
    let ap_fs = ref (mapi (fun i _ -> i, []) aps) in
    let curr_polar = List.map (fun _ -> ref true) aps in

    (*for i = 1 to (if !step_max >= 0 then !step_max else max_int) do*)
    while !curr_step < !step_max && !curr_time_l <= !time_max do
        (*Printf.printf "step %d at %s\n%!" !curr_step !curr_loc;*)
        report_step !curr_step !curr_loc;
        incr curr_step;

        List.map (set_param_ !curr_loc) ps;

        (* compute the earliest time reaching the inv frontier. *)
        let invs = Model.iexprs_of_loc (List.find (loc_of_name !curr_loc) locs) in
        let fs = mapi (find_inv_frontier_ !curr_loc) invs in
        let tmax = List.fold_left select_earliest None fs in
    
        (* find zero for each edge *)
        let es = Model.edges_of_loc (List.find (loc_of_name !curr_loc) locs) in
        let _,zsf,zs = List.fold_left (find_first_zero_ !curr_loc) (0,[],[]) es in
        let tmax = List.fold_left (select_earliest_grd !curr_loc invs es) (tmax,None) zsf in
        let zs = List.filter (filter_invariant !curr_loc invs es tmax) (List.append zsf zs) in
        (*let dst = List.fold_left select_earliest None zs in*)
        let dst = select_random zs in

        match dst with
        | Some (eid,(l0,u0)) ->

            (* update signal time intervals *)
            let fpf i = find_prop_frontier_ !curr_loc !curr_time_l u0 (List.nth curr_polar i) 
            in
            ap_fs := mapi fpf !ap_fs;

            (* FIXME *)
            find_first_zero true !curr_loc eid;
            if find_first_zero_mid !curr_loc eid then begin
                simulate_jump !curr_loc eid l0 u0;
                print_pped false false;
                curr_loc := dst_of_edge (List.nth es eid);
                curr_time_l := l0
            end else error FindZeroMidError;

            (*begin try 
                List.find (fun lid -> lid = !curr_loc) flocs;
                print_endline "reached final loc!"
            with Not_found -> ()
            end*)

        | None ->

            (* update signal time intervals *)
            let fpf i = find_prop_frontier_ !curr_loc !curr_time_l !time_max (List.nth curr_polar i) 
            in
            ap_fs := mapi fpf !ap_fs;

            simulate_cont !curr_loc;
            (*print_pped true true;
            dispose ();
            error FindZeroError*)
            curr_step := !step_max
    done;
    print_pped true true;
    dispose ();
    !ap_fs
