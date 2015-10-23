open Interval
open Capd_simulating_stubs
open Simulating
open Util

type polarity = Rise | Fall | Unknown

let print_polar fmt = function
    | Rise    -> Format.fprintf fmt "Rise"
    | Fall    -> Format.fprintf fmt "Fall"
    | Unknown -> Format.fprintf fmt "Unknown"


type flowpipe = (float * int * polarity) list


let check_prop_polar_ lid id (apid,_tlist) =
Printf.printf "cpp_ %s %d" lid id;
    let polar = match check_prop_polar lid id (* TODO *) with
        | 1 -> Rise
        | 0 -> Fall
        | _ -> (*error (CheckPropError (id,apid))*) Unknown
    in
Format.printf ": %a\n%!" print_polar polar;
    [0., [lid, id, polar]]

let invert_polar = function
    | Rise -> Fall
    | Fall -> Rise
    | _ -> Unknown

let rec find_prop_extremum_ tmax apid fp = 
    match List.nth fp ((List.length fp)-1) with
    | time_l, [lid, apid, polar] ->
        let (l,u) = find_prop_extremum lid apid time_l tmax in
        if l<=u && l > time_l then begin
Format.printf "fpe: %d [%f,%f]\n%!" apid l u;
            let fp = List.append fp [(l, [lid,apid,Unknown]); 
                                     (u, [lid,apid,invert_polar polar]) ] in
            find_prop_extremum_ tmax apid fp
        end else if l = -1. then
            error FindZeroError
        else
            fp
    | _ -> 
        assert false


let simulate (ps,_var,(iloc,_ival),locs) (aps,ap_locs) =
    let curr_step = ref 0 in
    let curr_loc = ref iloc in
    let curr_time_l = ref 0. in
    let _ = List.map (set_param_ !curr_loc) ps in
    initialize ();
    (*print_pped true false;*)

    (* initialize flowpipe *)
    let fps = ref (mapi (check_prop_polar_ !curr_loc) aps) in

    (*let ap_fs = List.map (fun ((apid,_), p) -> 
        let fs = if !p then [(Interval.zero, true)] else [] in
        apid, fs ) 
        (List.combine aps curr_polar) in
    let ap_fs = List.append ap_fs (mapi (fun i _lid -> i,[]) ap_locs) in
    let ap_fs = ref ap_fs in *)

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

(*            (* update signal time intervals *)
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
                simulate_jump !curr_loc eid l0 u0;
                print_pped false false;
                curr_loc := dst_of_edge (List.nth es eid);
                curr_time_l := l0
            end else error FindZeroMidError;
*)
            ()

        | None ->

            (* construct flowpipe *)
            fps := mapi (find_prop_extremum_ !time_max) !fps;

            simulate_cont !curr_loc !time_max;

            curr_step := !step_max
    done;
    print_pped true true;
    dispose ();
    (*Printf.printf "fpf: %d\n" !c_fpf;*)
    !fps
