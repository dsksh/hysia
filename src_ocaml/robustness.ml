open Model
open Interval
open Capd_simulating_stubs
open Simulating
open Util

type polarity = Rise | Fall | Unknown

let print_polar fmt = function
    | Rise    -> Format.fprintf fmt "Rise"
    | Fall    -> Format.fprintf fmt "Fall"
    | Unknown -> Format.fprintf fmt "Unknown"

let invert_polar = function
    | Rise -> Fall
    | Fall -> Rise
    | _ -> Unknown


type signal = Strue | Sfalse | Sexpr of string*int*polarity | Snot of signal

type flowpipe = (float * signal list) list

let rec print_signal fmt = function
    | Strue  -> Format.fprintf fmt "true"
    | Sfalse -> Format.fprintf fmt "false"
    | Sexpr (lid,apid,polar) -> 
            Format.fprintf fmt "expr[%s;%d;%a]" lid apid print_polar polar
    | Snot s -> Format.fprintf fmt "!(%a)" print_signal s

let rec print_signals fmt = function
    | []    -> () (*Format.fprintf fmt "    []"*)
    | s::[] -> Format.fprintf fmt "\n    %a" print_signal s
    | s::r  -> Format.fprintf fmt "\n    %a" print_signal s;
            print_signals fmt r
    | _ -> ()

let print_fp fmt fp = 
    let pr (t,ss) = Format.fprintf fmt "  %f:%a\n" t print_signals ss in
    let _ = List.map pr fp in 
    ()


let check_prop_polar_ lid id (apid,_tlist) =
Printf.printf "cpp_ %s %d" lid id;
    let polar = match check_prop_polar lid id (* TODO *) with
        | 1 -> Rise
        | 0 -> Fall
        | _ -> (*error (CheckPropError (id,apid))*) Unknown
    in
Format.printf ": %a\n%!" print_polar polar;
    apid, [0., [Sexpr (lid,id,polar)]]


let rec find_prop_extremum_ tmax (apid0, fp) = 
    match List.nth fp ((List.length fp)-1) with
    | time_l, [Sexpr (lid,apid,polar)] ->
        let (l,u) = find_prop_extremum lid apid time_l tmax in
        if l<=u && l > time_l then begin
Format.printf "fpe: %d [%f,%f]\n%!" apid l u;
            let fp = List.append fp [(l, [Sexpr (lid,apid,Unknown)]); 
                                     (u, [Sexpr (lid,apid,invert_polar polar)]) ] in
            find_prop_extremum_ tmax (apid0, fp)
        end else if l = -1. then
            error FindZeroError
        else
            (apid0, fp)
    | _ -> 
        assert false


let get_time_u tmax = function
    | (tu,_)::_ -> tu
    | [] -> tmax

let rec compare_signals_ neg1 neg2 tl tu = function
    | Sfalse, _      -> tu, [Sfalse]
    | _, Sfalse::_   -> tu, [Sfalse]
    | Strue, ss2     -> tu, ss2
    | s1, Strue::ss2 -> compare_signals_ neg1 false tl tu (s1,ss2)
    | s1, []         -> tu, [s1]
    | (Snot s1), ss2 ->
            compare_signals_ (not neg1) neg2 tl tu (s1,ss2)
    | s1, (Snot s2)::rest ->
            compare_signals_ neg1 (not neg2) tl tu (s1,s2::rest)
    | (Sexpr (lid1,_,_) as s1), (Sexpr (lid2,_,_))::rest when lid1 <> lid2 ->
            compare_signals_ neg1 false tl tu (s1,rest)

    | (Sexpr ( lid,apid1,polar1) as s1), 
     ((Sexpr (_lid,apid2,polar2))::rest as ss2) (* when lid = _lid *) ->
            let apid, (ctl,ctu) = compare_signals lid apid1 apid2 tl tu in
Format.printf "cs: %d, [%f,%f]\n%!" apid ctl ctu;
            let tu_ = if ctl > ctu (* no intersection *) then tu else ctl in
            if apid = apid1 then 
                compare_signals_ neg1 false tl tu_ (s1,rest)
            else if apid = apid2 then
                tu_, ss2
            else if ctl > ctu then (* unknown segment *)
                tu_, s1::ss2
            else (* intersection segment *)
                ctu, s1::ss2

let rec merge_fps tl tmax fp1 fp2 =
    match fp1, fp2 with
    | (tl1,ss1)::rest1, (tl2,ss2)::rest2 ->
        let tu1 = get_time_u tmax rest1 in
        let tu2 = get_time_u tmax rest2 in
        (*let tl = max tl0 (max tl1 tl2) in*)
        let tu,rest1,rest2 = if tu1 < tu2 then tu1,rest1,fp2 else tu2,fp1,rest2 in
        let tu_, ss = 
            List.fold_left 
                (fun (tu,ss2) s1 -> compare_signals_ false false tl tu (s1,ss2)) 
                (tu,ss2) ss1 in
        let rest = 
            if tu_ = tu then 
                merge_fps tu_ tmax rest1 rest2
            else
                merge_fps tu_ tmax fp1 fp2 
        in
        (tl,ss)::rest
    | fp1, [] -> []
    | [], fp2 -> []


let simulate (ps,_var,(iloc,_ival),locs) (aps,ap_locs) =
    let curr_step = ref 0 in
    let curr_loc = ref iloc in
    let curr_time_l = ref 0. in
    let _ = List.map (set_param_ !curr_loc) ps in
    initialize ();
    (*print_pped true false;*)

    (* initialize flowpipe *)
    let ap_fps = ref (mapi (check_prop_polar_ !curr_loc) aps) in

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
            ap_fps := List.map (find_prop_extremum_ !time_max) !ap_fps;

            simulate_cont !curr_loc !time_max;

            curr_step := !step_max
    done;
    print_pped true true;
    dispose ();
    (*Printf.printf "fpf: %d\n" !c_fpf;*)
    !ap_fps

let negate_signal = function
    | Strue  -> Sfalse
    | Sfalse -> Strue
    | Sexpr _ as s -> Snot s
    | Snot s -> s

let rec propagate debug ap_fps = function
    | Mtrue -> if debug then Printf.printf "  true\n"; 
        [0., [Strue]]
    (*| Mloc (id,_lid) ->*)
    | Mexpr d -> 
        let fp = snd (List.find (fun (apid,_fp) -> apid = d.Hashcons.tag) ap_fps) in
        if debug then Format.printf "  expr %d\n%a" d.Hashcons.tag print_fp fp;
        fp
    | Mnot f -> 
        let fp = propagate debug ap_fps f in
        let fp = List.map (fun (t,ss) -> t, List.map negate_signal ss) fp in
        if debug then Format.printf "  not\n%a" print_fp fp;
        fp
    | Mand (f1,f2) -> 
        let fp = merge_fps 0. !time_max (propagate debug ap_fps f1) 
                                        (propagate debug ap_fps f2) in
        if debug then Format.printf "  and\n%a" print_fp fp;
        fp
    (*| Muntil (t,f1,f2) -> 
        let bs1 = propagate debug ap_bs f1 in
        let bs2 = propagate debug ap_bs f2 in
        let bs  = shift_bs t bs1 bs2 in
        if debug then Format.printf "  until %a\n%a" print_interval t print_bs bs;
        bs
    *)
    | _ ->
        assert false

