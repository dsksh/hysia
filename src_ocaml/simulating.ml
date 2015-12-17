open Interval
open Capd_simulating_stubs
open Util

let step_max = ref max_int
let time_max = ref infinity

let loc_of_name id loc = id = Model.id_of_loc loc

let find_inv_frontier_ lid iid _inv = 
(*Printf.printf "fif %s %d\n%!" lid iid;*)
    iid, (find_inv_frontier lid iid)

let select_earliest earliest (id,(l,u)) = 
    if l>u then 
        earliest
    else if l < 0. then
        error FindZeroError
    else
    match earliest with
    | Some (id1,(l1,u1)) ->
(*Printf.printf "1: %d,[%f,%f] vs. %d,[%f,%f]\n%!" id1 l1 u1 id l u;*)
        if u < l1 then Some (id,(l,u)) else begin
            if u1 < l then Some (id1,(l1,u1)) 
            else error (SelectEarliestError ((l,u), (l1,u1)))
        end 
    | None -> 
        Some (id,(l,u))


let find_first_zero_ lid (forced,_,_,_,_) (eid,zsf,zs) = 
    let (l,u) = find_first_zero false lid eid in
(*Printf.printf "0: %d,[%f,%f]\n%!" eid l u;*)
    if l<=u && l>=0. then begin
        if forced then
            eid+1, (eid,(l,u))::zsf, zs
        else
            eid+1, zsf, (eid,(l,u))::zs
    end else if l>u then 
        eid+1, zsf, zs
    else (* if l = -1. then *)
        error FindZeroError

let select_earliest_grd _lid invs es earliest (eid,(l,u)) =
  match earliest with
  | Some (iid,(l1,u1)), _None ->
(*Printf.printf "2: %d,[%f,%f] vs. %d,[%f,%f]\n%!" iid l1 u1 eid l u;*)

      let (_,gh,_,_,_) = List.nth es eid in
(*Printf.printf "%b\n" (fst (List.nth invs iid) = gh);*)
      if (fst (List.nth invs iid) = gh) then 
        None, Some (eid,(l,u))

      else begin
        (*if l<=u && l>=0. then begin*)
          if u < l1 then None, Some (eid,(l,u)) else begin
            if u1 < l then Some (iid,(l1,u1)), None else 
              error (SelectEarliestError ((l,u), (l1,u1))) end 
        (*end 
        else if l = -1. then
          error (SelectEarliestError ((l,u), (l1,u1)))
        else
          Some (iid,(l1,u1)), None
        *)
      end

  | _None, Some (eid1,(l1,u1)) ->
(*Printf.printf "3: %d,[%f,%f] vs. %d,[%f,%f]\n%!" eid1 l1 u1 eid l u;*)
      (*if l<=u && l>=0. then begin*)
        if u < l1 then None, Some (eid,(l,u)) else begin
          if u1 < l then None, Some (eid1,(l1,u1)) else 
            error (SelectEarliestError ((l,u), (l1,u1))) end 
      (*end 
      else if l = -1. then
        error (SelectEarliestError ((l,u), (l1,u1)))
      else
        None, Some (eid1,(l1,u1))
      *)

  | None, None -> 
(*Printf.printf "4: %d,[%f,%f]\n%!" eid l u;*)
      (*if l<=u && l>=0. then None, Some (eid,(l,u)) else None, None*)
      None, Some (eid,(l,u))

let filter_invariant _lid invs es tmax (eid,(l,u)) =
    match tmax with
    | Some (iid,(lm,_um)), _None -> 
(*Printf.printf "4: %d,[%f,%f] vs. %d,[%f,%f]\n%!" iid lm um eid l u;*)
        let (_,gh,_,_,_) = List.nth es eid in
(*Printf.printf "%b\n%!" (fst (List.nth invs iid) = gh);*)
        if (fst (List.nth invs iid) = gh) then true
        else 
          l<=u && u < lm

    | _None, Some t when t = (eid,(l,u)) -> true
    | _None, Some (_,(lm,_um)) -> l<=u && u < lm

    | None, None -> l<=u


let select_random dst_list = 
    let dl = List.filter (fun (_eid,(l,u)) -> l<=u) dst_list in
    match dl with
    | [] -> None
    | dl -> Some (List.nth dl (Random.int (List.length dl)))

let dst_of_edge (_,_,_,dst,_) = dst

let set_param_ lid (id,bnd) = 
  set_param lid id (Random.float bnd)

(*let c_fpf = ref 0*)


(* APs on the C/C++ side correspond to the indexes (id) of the AP list. 
 * On the OCaml side, APs should be paired with apid. 
 *)
let check_prop_ lid id (apid,tlist) =
  let res = check_prop lid id (* TODO *) in
  match res with
  | 1 -> ref true
  | 0 -> ref false
  | _ -> error (CheckPropError (id,apid))

let find_prop_frontier_ lid t0 tmax polar id (apid,tlist) =
(*Printf.printf "fpf_ %d %f %f\n%!" apid t0 tmax;*)
  let tlist = ref tlist in
  let time_l = ref t0 in
  (*let polar = ref polar in*)
  while !time_l >= 0. && !time_l < tmax do
(*let _ = incr c_fpf in*)
    let (l,u) = find_prop_frontier lid id !polar !time_l tmax in
    (*let (l,u) = find_prop_frontier lid id true !time_l tmax in*)
(*Printf.printf "fpf %f %f %f\n%!" l u !time_l;*)
    if l<=u && l > !time_l then begin
      (*time_l := l;*)
      time_l := u;
      polar := not !polar;
      tlist := List.append !tlist [({inf=l;sup=u},!polar)]
    end else if l = -1. then
      error FindZeroError
    else
      time_l := -1.;
  done;
  apid, !tlist

let simulate (ps,_var,(iloc,_ival),locs) (aps,ap_locs) =
    let curr_step = ref 0 in
    let curr_loc = ref iloc in
    let curr_time_l = ref 0. in
    let _ = List.map (set_param_ !curr_loc) ps in
    initialize ();
    print_pped true false;

    (* initialize ap boundaries list. *)
    let curr_polar = mapi (check_prop_ !curr_loc) aps in
    (*let curr_polar = List.map (fun _ -> ref true) aps in*)
    (*let ap_fs = List.map (fun (apid,_) -> apid, [(Interval.zero, true)]) aps in*)
    let ap_fs = List.map (fun ((apid,_), p) -> 
        let fs = if !p then [(Interval.zero, true)] else [] in
        apid, fs ) 
        (List.combine aps curr_polar) in
    let ap_fs = List.append ap_fs (mapi (fun i _lid -> i,[]) ap_locs) in
    let ap_fs = ref ap_fs in

    while !curr_step < !step_max && !curr_time_l <= !time_max do
Printf.printf "step %d (%f < %f) at %s\n%!" !curr_step !curr_time_l !time_max !curr_loc;
        report_step !curr_step !curr_loc;
        incr curr_step;

        let _ = List.map (set_param_ !curr_loc) ps in

        (* compute the earliest time reaching the inv frontier. *)
        let invs = Model.iexprs_of_loc (List.find (loc_of_name !curr_loc) locs) in
        let fs = mapi (find_inv_frontier_ !curr_loc) invs in
        let tmax = List.fold_left select_earliest None fs in
    
        (* find zero for each edge *)
        let es = Model.edges_of_loc (List.find (loc_of_name !curr_loc) locs) in
        let _,zsf,zs = List.fold_right (find_first_zero_ !curr_loc) es (0,[],[]) in
        let tmax = List.fold_left (select_earliest_grd !curr_loc invs es) (tmax,None) zsf in
        let zs = List.filter (filter_invariant !curr_loc invs es tmax) (List.append zsf zs) in
        (*let dst = List.fold_left select_earliest None zs in*)
        let dst = select_random zs in

        match dst with
        | Some (eid,(l0,u0)) ->

            (* update signal time intervals *)
            let fpf i (apid,tlist) = 
                let n_aps = List.length aps in
                if i < n_aps then 
                    find_prop_frontier_ !curr_loc !curr_time_l u0 
                        (List.nth curr_polar i) i (apid,tlist)
                else begin
                    (* i > n_aps: indexes are used for location APs *)
                    let lid = List.nth ap_locs apid in
                    let dst = dst_of_edge (List.nth es eid) in
                    if !curr_loc = lid && dst <> lid then
                        apid, List.append tlist [({inf=l0;sup=u0},false)]
                    else begin if dst = lid then
                        apid, List.append tlist [({inf=l0;sup=u0},true)]
                    else 
                        apid, tlist end
                end
            in
            ap_fs := mapi fpf !ap_fs;

            (* FIXME *)
            let _ = find_first_zero true !curr_loc eid in
            if find_first_zero_mid !curr_loc eid then begin
                (*simulate_jump !curr_loc eid l0 u0;*)
                simulate_jump !curr_loc eid l0 u0;
                print_pped false false;
                curr_loc := dst_of_edge (List.nth es eid);
                curr_time_l := l0
            end else error FindZeroMidError;

        | None ->

            (* update signal time intervals *)
            let fpf i = find_prop_frontier_ !curr_loc !curr_time_l !time_max (List.nth curr_polar i) i
            in
            ap_fs := mapi fpf !ap_fs;

            simulate_cont !curr_loc !time_max;
            (*print_pped true true;
            dispose ();
            error FindZeroError*)
            curr_step := !step_max
    done;
    print_pped true true;
    dispose ();
(*Printf.printf "fpf: %d\n" !c_fpf;*)
    !ap_fs
