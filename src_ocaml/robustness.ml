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


type signal = 
    | Strue 
    | Sfalse 
    | Sconst of float*float 
    | Sexpr of string*int*polarity 
    | Snot of signal

type flowpipe = (float * signal list) list

let rec print_signal fmt = function
    | Strue  -> Format.fprintf fmt "true"
    | Sfalse -> Format.fprintf fmt "false"
    | Sconst (vl,vu) -> 
            Format.fprintf fmt "const[%f;%f]" vl vu
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


let negate_signal = function
    | Strue  -> Sfalse
    | Sfalse -> Strue
    | Sconst _ as s -> Snot s
    | Sexpr _ as s -> Snot s
    | Snot s -> s


let check_prop_polar_ lid id (apid,_tlist) =
Printf.printf "cpp_ %s %d\n%!" lid id;
    match check_prop_kind lid id with
    | 2, (vl,vu) -> 
let _ = Format.printf ": Flat\n%!" in
        apid, [0., [Sconst (vl,vu)]] 
    | kind, _ ->
        let polar = match kind with
        | 1 -> Rise
        | 0 -> Fall
        | _ -> (*error (CheckPropError (id,apid))*) Unknown
        in
Format.printf ": %a\n%!" print_polar polar;
        apid, [0., [Sexpr (lid,id,polar)]]


let rec find_prop_extremum_ tmax (apid0, fp) = 
    match List.nth fp ((List.length fp)-1) with
    | time_l, [Sconst _] ->
        (apid0, fp)
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

let rec minimize_signals_ neg1 neg2 tl tu = function
    | Sfalse, ss2      -> 
            if neg1 then
                minimize_signals_ false neg2 tl tu (Strue,ss2)
            else 
                tu, [Sfalse]
    | s1, Sfalse::ss2   -> 
            if neg2 then
                minimize_signals_ neg1 false tl tu (s1,Strue::ss2)
            else
                tu, [Sfalse]
    | Strue, (s2::rest as ss2) -> 
            if neg1 then
                minimize_signals_ false neg2 tl tu (Sfalse,ss2)
            else
                tu, if neg2 then (Snot s2)::rest else ss2
    | s1, Strue::ss2 -> 
            if neg2 then
                minimize_signals_ neg1 false tl tu (s1,Sfalse::ss2)
            else
                minimize_signals_ neg1 false tl tu (s1,ss2)

    | s1, []         -> 
            tu, [if neg1 then Snot s1 else s1]

    | (Snot s1), ss2 ->
            minimize_signals_ (not neg1) neg2 tl tu (s1,ss2)
    | s1, (Snot s2)::rest ->
            minimize_signals_ neg1 (not neg2) tl tu (s1,s2::rest)

    | (Sconst (vl1,vu1) as s1), 
      (Sconst (vl2,vu2) as s2)::rest ->
            let vl1,vu1 = if neg1 then (-.vl1),(-.vu1) else vl1,vu1 in
            let vl2,vu2 = if neg2 then (-.vl2),(-.vu2) else vl2,vu2 in
            let s1 = Sconst (vl1,vu2) in
            let ss2 = (Sconst (vl2,vu2))::rest in

            if vu1 < vl2 then
                minimize_signals_ neg1 false tl tu (s1,rest)
            else if vl1 > vu2 then
                tu, ss2
            else (* overlap; unknown segment *)
                tu, s1::ss2

    |  Sconst (vl,vu), ((Sexpr (lid,apid,_polar) as s2)::rest as ss2) ->
            let vl,vu = if neg1 then (-.vl),(-.vu) else vl,vu in
            let s1 = Sconst (vl,vu) in

            let res, (ctl,ctu) = find_intersection lid neg2 apid vl vu tl tu in
            let tu = if ctl > ctu (* no intersection *) then tu else ctl in
            if res > apid then (* ap is above vu *)
                minimize_signals_ false false tl tu (s1,rest)
            else
                let ss2 = if neg2 then (Snot s2)::rest else ss2 in

                if res = apid then
                    tu, ss2
                else if ctl > ctu then (* unknown segment *)
                    tu, s1::ss2
                else (* intersection segment *)
                    ctu, s1::ss2

    | (Sexpr ( lid,apid,_polar) as s1), (Sconst (vl,vu))::rest ->
            let vl,vu = if neg2 then (-.vl),(-.vu) else vl,vu in
            let ss2 = (Sconst (vl,vu))::rest in

            let res, (ctl,ctu) = find_intersection lid neg1 apid vl vu tl tu in
            let tu = if ctl > ctu (* no intersection *) then tu else ctl in
            if res = apid then (* ap is below vu *)
                minimize_signals_ neg1 false tl tu (s1,rest)
            else
                let s1 = if neg1 then Snot s1 else s1 in

                if res > apid then
                    tu, ss2
                else if ctl > ctu then (* unknown segment *)
                    tu, s1::ss2
                else (* intersection segment *)
                    ctu, s1::ss2

    | (Sexpr (lid1,_,_) as s1), (Sexpr (lid2,_,_))::rest when lid1 <> lid2 -> (* TODO *)
            minimize_signals_ neg1 false tl tu (s1,rest)

    | (Sexpr ( lid,apid1,_polar1) as s1), 
     ((Sexpr (_lid,apid2,_polar2) as s2)::rest as ss2) (* when lid = _lid *) ->
            let apid, (ctl,ctu) = compare_signals lid neg1 neg2 apid1 apid2 tl tu in
Format.printf "cs: %d, [%f,%f]\n%!" apid ctl ctu;
            let tu = if ctl > ctu (* no intersection *) then tu else ctl in
            if apid = apid1 then 
                minimize_signals_ neg1 false tl tu (s1,rest)
            else
                let s1 = if neg1 then Snot s1 else s1 in
                let ss2 = if neg2 then (Snot s2)::rest else ss2 in

                if apid = apid2 then
                    tu, ss2
                else if ctl > ctu then (* unknown segment *)
                    tu, s1::ss2
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
                (fun (tu,ss2) s1 -> minimize_signals_ false false tl tu (s1,ss2)) 
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


let rec value_at_ t neg = function
    | Strue -> 
            (infinity,infinity)
    | Sfalse -> 
            (neg_infinity,neg_infinity)
    | Snot s -> 
            let vl,vu = value_at_ t (not neg) s in (-.vl, -.vu)
    | Sconst (vl,vu) -> 
            (vl,vu)
    | Sexpr (lid,apid,_) ->
            value_at t neg lid apid

let compare_vs (l1,u1) (l2,u2) =
    if l1 > u2 then 1
    else if u1 < l2 then 0
    else -1

let find_intersection_ neg1 neg2 s1 s2 tl tu = match s1 with
    | Sexpr (lid,apid1,_) -> begin 
        match s2 with
        | Sconst (vl,vu) ->
            let _, (ctl,ctu) = find_intersection lid neg1 apid1 vl vu tl tu in
            (ctl,ctu)
        | Sexpr (_,apid2,_) -> (* TODO: check whether lid's matches? *)
            let _, (ctl,ctu) = compare_signals lid neg1 neg2 apid1 apid2 tl tu in
            (ctl,ctu)
        | _ ->
            (* TODO *) assert false
    end
    | _ ->
        (* TODO *) assert false


let rec proc_evt_ut_ neg tl tu rest z = function
    | [] -> (tl,[z])::rest, z
    | Snot s::ss ->
            proc_evt_ut_ (not neg) tl tu rest z (s::ss)

    | Sfalse::ss -> 
            if neg then
                (tl,[Strue])::rest, Strue
            else
                proc_evt_ut_ false tl tu rest z ss

    | Strue::ss -> 
Format.printf "peu: true\n%!";
            if neg then
                proc_evt_ut_ false tl tu rest z ss
            else
                (tl,[Strue])::rest, Strue

    | (Sconst (vl,vu))::ss ->
Format.printf "peu: const [%f;%f]\n%!" vl vu;
            let zv = value_at_ tu false z in
            let vl,vu = if neg then ((-.vl),(-.vu)) else (vl,vu) in
            let s = Sconst (vl,vu) in
            begin match compare_vs (vl,vu) zv with
            | 0 -> proc_evt_ut_ false tl tu rest z ss
            | 1 -> proc_evt_ut_ false tl tu rest s ss
            | _ -> proc_evt_ut_ false tl tu rest z ss (* TODO *)
            end

    | ((Sexpr (lid,apid,Rise) as s)::ss as ss0) ->
Format.printf "peu: expr rise %b %f\n%!" neg tl;
            let zvu = value_at_ tu false z in
            let vu = value_at_ tu neg s in
            let res = compare_vs vu zvu in
Format.printf "res: %d\n%!" res;
            begin if neg then (* Fall *)
                match res with
                | 1 -> proc_evt_ut_ false tl tu rest (Snot s) ss
                | 0 ->  begin
                    let zvl = value_at_ tl false z in
                    let vl = value_at_ tl neg s in
                    let res = compare_vs vl zvl in
Format.printf "res: %d\n%!" res;
                    match res with
                    | 0 -> proc_evt_ut_ false tl tu rest z ss
                    | _ ->
                        let ctl,ctu = find_intersection_ neg false s z tl tu in
(*Format.printf "cs_: %d, [%f,%f]\n%!" apid ctl ctu;*)
                        let tu, rest, ss = 
                            if ctl > ctu (* no intersection *) then 
                                tu, rest, ss
                            else 
                                ctl, (ctl,[z])::rest, ss0 in
                        proc_evt_ut_ false tl tu rest z ss
                    end
                | _ -> proc_evt_ut_ false tl tu rest s ss (* TODO *)
            else (* Rise *)
                let s = Sconst (fst vu, snd vu) in
                match res with
                | 0 -> proc_evt_ut_ false tl tu rest z ss
                | 1 -> proc_evt_ut_ false tl tu rest s ss
                | _ -> proc_evt_ut_ false tl tu rest z ss (* TODO *)
            end

    | ((Sexpr (lid,apid,Fall) as s)::ss as ss0) ->
Format.printf "peu: expr fall %b %f\n%!" neg tl;
            let zvu = value_at_ tu false z in
            let vu = value_at_ tu neg s in
            let res = compare_vs vu zvu in
Format.printf "res: %d\n%!" res;
            begin if neg then (* Rise *)
                let s = Sconst (fst vu, snd vu) in
                match res with
                | 0 -> proc_evt_ut_ false tl tu rest z ss
                | 1 -> proc_evt_ut_ false tl tu rest (Snot s) ss
                | _ -> proc_evt_ut_ false tl tu rest z ss (* TODO *)
            else (* Fall *)
                match res with
                | 1 -> proc_evt_ut_ false tl tu rest s ss
                | 0 ->  begin
                    let zvl = value_at_ tl false z in
                    let vl = value_at_ tl neg s in
                    let res = compare_vs vl zvl in
Format.printf "res: %d\n%!" res;
                    match res with
                    | 0 -> proc_evt_ut_ false tl tu rest z ss
                    | _ ->
                        let ctl,ctu = find_intersection_ neg false s z tl tu in
Format.printf "peu_: [%f,%f]\n%!" ctl ctu;
                        let tu, rest, ss = 
                            if ctl > ctu (* no intersection *) then 
                                tu, rest, ss
                            else 
                                ctl, (ctl,[z])::rest, ss0 in
                        proc_evt_ut_ false tl tu rest z ss
                    end
                | _ -> proc_evt_ut_ false tl tu rest s ss (* TODO *)
            end

    | (Sexpr (lid,apid,Unknown) as s)::ss ->
Format.printf "peu: expr unk %b %f\n%!" neg tl;
            let zv = value_at_ tu false z in
            let v = value_at_ tu neg s in
            begin match compare_vs v zv with
            | 0 -> proc_evt_ut_ neg tl tu rest (Sconst (fst zv,snd zv)) ss
            | 1 -> proc_evt_ut_ neg tl tu rest (Sconst (fst v, snd v)) ss
            | _ -> proc_evt_ut_ neg tl tu rest (Sconst (fst zv,snd zv)) ss (* TODO *)
            end


let proc_evt_ut tmax (tl,ss) (rest,z) = 
    let tu = get_time_u tmax rest in
    proc_evt_ut_ false tl tu rest z ss


(* preserve the parameter values. *)
let param_values = Queue.create ()

let get_param_value (id,bnd) =
    id, (Random.float bnd)

let set_param_ lid (id,value) = 
    set_param lid id value


let simulate (ps,_var,(iloc,_ival),locs) (aps,ap_locs) =
    let curr_step = ref 0 in
    let curr_loc = ref iloc in
    let curr_time_l = ref 0. in

    let pvs = List.map get_param_value ps in
    Queue.add pvs param_values;
    let _ = List.map (set_param_ !curr_loc) pvs in

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

        let pvs = List.map get_param_value ps in
        Queue.add pvs param_values;
        let _ = List.map (set_param_ !curr_loc) pvs in

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

            (*simulate_cont !curr_loc !time_max;*)

            curr_step := !step_max
    done;
    print_pped true true;
    dispose ();
    (*Printf.printf "fpf: %d\n" !c_fpf;*)
    !ap_fps


let rec propagate debug ap_fps = function
    | Mtrue -> if debug then Printf.printf "  true\n"; 
        [0., [Strue]]
    (*| Mloc (id,_lid) ->*)
    | Mexpr d -> 
print_endline "expr";
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
    | Mevt_ut f ->
print_endline "evt_ut";
        let fp = propagate debug ap_fps f in
print_endline "peu";
        let fp,_ = List.fold_right (proc_evt_ut !time_max) fp ([],Sfalse) in
        if debug then Format.printf "  evt_ut\n%a" print_fp fp;
        fp
    (*| Muntil (t,f1,f2) -> 
        let bs1 = propagate debug ap_bs f1 in
        let bs2 = propagate debug ap_bs f2 in
        let bs  = shift_bs t bs1 bs2 in
        if debug then Format.printf "  until %a\n%a" print_interval t print_bs bs;
        bs
    *)
    | _ ->
print_endline "unsupported";
        assert false


(* TODO: dump const *)
let rec dump_signal is_neg tl tu = function
    | Strue  -> dump_bool is_neg tl tu
    | Sfalse -> dump_bool (not is_neg) tl tu
    | Sconst (vl,vu) ->
            dump_const is_neg vl vu tl tu
    | Sexpr (lid,apid,_polar) -> 
            dump_ap lid apid is_neg tl tu
    | Snot s -> 
            dump_signal (not is_neg) tl tu s
    | _ -> ()

let rec dump_signals tmax = function 
    | (tl,ss)::rest ->
      let tu = get_time_u tmax rest in
      let _ = List.map (dump_signal false tl tu) ss in
      dump_signals tmax rest

    | [] -> ()

let dump_fp (ps,_var,(iloc,_ival),locs) fp (*(aps,ap_locs)*) =
    (* turn on the dump function. *)
    Capd_sending_stubs.set_solving_param "dump_to_file" 1.;

    let curr_step = ref 0 in
    let curr_loc = ref iloc in
    let curr_time_l = ref 0. in

    let pvs = Queue.take param_values in
    let _ = List.map (set_param_ !curr_loc) pvs in

    initialize ();

    dump_signals !time_max fp;

    (* TODO *)
    print_pped true true;
    dispose ();
    ()

