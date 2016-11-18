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
    | Sconst of Interval.t
    | Sexpr of string*int*polarity 
    | Snot of signal
    | Sshift of float*signal

type flowpipe = (float * signal list) list

let rec print_signal fmt = function
    | Strue  -> Format.fprintf fmt "true"
    | Sfalse -> Format.fprintf fmt "false"
    | Sconst v -> 
            Format.fprintf fmt "const%a" print_interval v
    | Sexpr (lid,apid,polar) -> 
            Format.fprintf fmt "expr[%s;%d;%a]" lid apid print_polar polar
    | Snot s -> Format.fprintf fmt "!(%a)" print_signal s
    | Sshift (st,s) -> Format.fprintf fmt "(%a-%f)" print_signal s st

let rec print_signals fmt = function
    | []    -> () (*Format.fprintf fmt "    []"*)
    | s::[] -> Format.fprintf fmt "\n    %a" print_signal s
    | s::r  -> Format.fprintf fmt "\n    %a" print_signal s;
            print_signals fmt r

let print_fp fmt fp = 
    let pr (t,ss) = Format.fprintf fmt "  %f:%a\n" t print_signals ss in
    let _ = List.map pr fp in 
    ()


let proc_not = function
    | Strue  -> Sfalse
    | Sfalse -> Strue
    | Snot s -> s
    (*| Sconst _ as s -> Snot s
    | Sexpr _ as s -> Snot s*)
    | s -> Snot s


let check_prop_polar_ lid id (apid,_tlist) =
(*Printf.printf "cpp_ %s %d\n%!" lid id;*)
    match check_prop_kind lid id with
    | 2, (vl,vu) -> 
        apid, [0., [Sconst {inf=vl;sup=vu}]] 
    | kind, _ ->
        let polar = match kind with
        | 1 -> Rise
        | 0 -> Fall
        | _ -> (*error (CheckPropError (id,apid))*) Unknown
        in
(*Format.printf ": %a\n%!" print_polar polar;*)
        apid, [0., [Sexpr (lid,id,polar)]]


(* check the extremum points and segment if necessary *)
let rec find_prop_extremum_ tmax (apid0, fp) = 
    match List.nth fp ((List.length fp)-1) with
    | _time_l, [Sconst _] ->
        (apid0, fp)
    | time_l, [Sexpr (lid,apid,polar)] ->
        let (l,u) = find_prop_extremum lid apid time_l tmax in
        if l<=u && l > time_l then begin
(*Format.printf "fpe: %d [%f,%f]\n%!" apid l u;*)
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

let rec minimize_signals_ neg0 neg1 neg2 st1 st2 tl tu = function
    | Sfalse, ss2      -> 
            if neg1 then
                minimize_signals_ neg0 neg0 neg2 st1 st2 tl tu (Strue,ss2)
            else 
                tu, [Sfalse]
    | s1, Sfalse::ss2   -> 
            if neg2 then
                minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,Strue::ss2)
            else
                tu, [Sfalse]
    | Strue, (s2::rest as ss2) -> 
            if neg1 then
                minimize_signals_ neg0 neg0 neg2 st1 st2 tl tu (Sfalse,ss2)
            else
                tu, if neg2 then (Snot s2)::rest else ss2
    | s1, Strue::ss2 -> 
            if neg2 then
                minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,Sfalse::ss2)
            else
                minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,ss2)

    | s1, []         -> 
            tu, [if neg1 then Snot s1 else s1]

    | (Snot s1), ss2 ->
            minimize_signals_ neg0 (not neg1) neg2 st1 st2 tl tu (s1,ss2)
    | s1, (Snot s2)::rest ->
            minimize_signals_ neg0 neg1 (not neg2) st1 st2 tl tu (s1,s2::rest)

    | Sshift (st1_,s1), ss2 ->
            minimize_signals_ neg0 neg1 neg2 (st1+.st1_) st2 tl tu (s1,ss2)
    | s1, Sshift (st2_,s2)::rest ->
            minimize_signals_ neg0 neg1 neg2 st1 (st2+.st2_) tl tu (s1,s2::rest)

    | (Sconst v1), (Sconst v2)::rest ->
            let v1 = if neg1 then (-$)v1 else v1 in
            let v2 = if neg2 then (-$)v2 else v2 in
            let s1 = Sconst {inf=v1.inf; sup=v2.sup} in
            let ss2 = (Sconst v2)::rest in

            if v1.sup < v2.inf then
                minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,rest)
            else if v1.inf > v2.sup then
                tu, ss2
            else (* overlap; unknown segment *)
                tu, s1::ss2

    |  Sconst v, ((Sexpr (lid,apid,_polar) as s2)::rest as ss2) ->
            let v = if neg1 then (-$)v else v in
            let s1 = Sconst v in

            let res, (ctl,ctu) = find_intersection lid neg2 st2 apid v.inf v.sup tl tu in
            let tu = if ctl > ctu (* no intersection *) then tu else ctl in
            if res > apid then (* ap is above vu *)
                minimize_signals_ neg0 neg0 neg0 st1 st2 tl tu (s1,rest)
            else
                let ss2 = if neg2 then (Snot s2)::rest else ss2 in

                if res = apid then
                    tu, ss2
                else if ctl > ctu then (* unknown segment *)
                    tu, s1::ss2
                else (* intersection segment *)
                    ctu, s1::ss2

    | (Sexpr ( lid,apid,_polar) as s1), (Sconst v)::rest ->
            let v = if neg2 then (-$)v else v in
            let ss2 = (Sconst v)::rest in

            let res, (ctl,ctu) = find_intersection lid neg1 st1 apid v.inf v.sup tl tu in
            let tu = if ctl > ctu (* no intersection *) then tu else ctl in
            if res = apid then (* ap is below vu *)
                minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,rest)
            else
                let s1 = if neg1 then Snot s1 else s1 in

                if res > apid then
                    tu, ss2
                else if ctl > ctu then (* unknown segment *)
                    tu, s1::ss2
                else (* intersection segment *)
                    ctu, s1::ss2

    | (Sexpr (lid1,_,_) as s1), (Sexpr (lid2,_,_))::rest when lid1 <> lid2 -> (* TODO *)
            minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,rest)

    | (Sexpr ( lid,apid1,_polar1) as s1), 
     ((Sexpr (_lid,apid2,_polar2) as s2)::rest as ss2) (* when lid = _lid *) ->
            let apid, (ctl,ctu) = compare_signals lid neg1 neg2 st1 st2 apid1 apid2 tl tu in
(*Format.printf "compare_signals: %f %f\n%!" ctl ctu;*)
            let tu = if ctl > ctu (* no intersection *) then tu else ctl in
            if apid = apid1 then 
                minimize_signals_ neg0 neg1 neg0 st1 st2 tl tu (s1,rest)
            else
                let s1 = if neg1 then Snot s1 else s1 in
                let ss2 = if neg2 then (Snot s2)::rest else ss2 in

(*Format.printf "apids: %d %d %d\n%!" apid apid1 apid2;*)
                if apid = apid2 then
                    tu, ss2
                else if ctl > ctu then (* unknown segment *)
                    tu, s1::ss2
                else (* intersection segment *) 
                    ctu, s1::ss2

let rec proc_and tl tmax fp1 fp2 =
    match fp1, fp2 with
    | (_tl1,ss1)::rest1, (_tl2,ss2)::rest2 ->
        let tu1 = get_time_u tmax rest1 in
        let tu2 = get_time_u tmax rest2 in
        (*let tl = max tl0 (max tl1 tl2) in*)
        let tu,rest1,rest2 = if tu1 < tu2 then tu1,rest1,fp2 else tu2,fp1,rest2 in
        let tu_, ss = 
            List.fold_left 
                (fun (tu,ss2) s1 -> minimize_signals_ false false false 0. 0. tl tu (s1,ss2)) 
                (tu,ss2) ss1 in
        let rest = 
            if tu_ = tu then 
                proc_and tu_ tmax rest1 rest2
            else
                proc_and tu_ tmax fp1 fp2 
        in
        (tl,ss)::rest
    | _fp1, [] -> []
    | [], _fp2 -> []

let rec proc_or tl tmax fp1 fp2 =
    match fp1, fp2 with
    | (_tl1,ss1)::rest1, (_tl2,ss2)::rest2 ->
        let tu1 = get_time_u tmax rest1 in
        let tu2 = get_time_u tmax rest2 in
        let tu,rest1,rest2 = if tu1 < tu2 then tu1,rest1,fp2 else tu2,fp1,rest2 in
        let tu_, ss = 
            List.fold_left 
                (fun (tu,ss2) s1 -> minimize_signals_ true true true 0. 0. tl tu (s1,ss2)) 
                (tu,ss2) ss1 in
        let ss = List.map proc_not ss in
        let rest = 
            if tu_ = tu then 
                proc_or tu_ tmax rest1 rest2
            else
                proc_or tu_ tmax fp1 fp2 
        in
        (tl,ss)::rest
    | _fp1, [] -> []
    | [], _fp2 -> []


let rec value_at_ t neg = function
    | Strue -> 
            if neg then {inf=neg_infinity;sup=neg_infinity} else {inf=infinity;sup=infinity}
    | Sfalse -> 
            if neg then {inf=infinity;sup=infinity} else {inf=neg_infinity;sup=neg_infinity}
    | Snot s -> 
            value_at_ t (not neg) s
    | Sconst v -> 
            if neg then (-$)v else v
    | Sexpr (lid,apid,_) ->
            let v = value_at t neg lid apid in
            {inf=fst v; sup=snd v}
    | Sshift (st,s) ->
            value_at_ (t-.st) neg s

let compare_vs v1 v2 =
    if v1.inf > v2.sup then 1
    else if v1.sup < v2.inf then 0
    else -1

let find_intersection_ neg1 neg2 s1 s2 tl tu = match s1 with
    | Sexpr (lid,apid1,_) -> begin 
        match s2 with
        | Sconst v ->
            let _, (ctl,ctu) = find_intersection lid neg1 0. apid1 v.inf v.sup tl tu in
            (ctl,ctu)
        | Sexpr (_,apid2,_) -> (* TODO: check whether lid's matches? *)
            let _, (ctl,ctu) = compare_signals lid neg1 neg2 0. 0. apid1 apid2 tl tu in
            (ctl,ctu)
        | _ ->
            (* TODO *) assert false
    end
    | _ ->
        (* TODO *) assert false


let rec proc_evt_ut_ debug neg tl tu rest z = function
    | [] -> (tl,[z])::rest, z

    | Snot s::ss ->
            proc_evt_ut_ debug (not neg) tl tu rest z (s::ss)

    | Sfalse::ss -> 
            if neg then
                (tl,[Strue])::rest, Strue
            else
                proc_evt_ut_ debug false tl tu rest z ss

    | Strue::ss -> 
if debug then Format.printf "peu: true\n%!";
            if neg then
                proc_evt_ut_ debug false tl tu rest z ss
            else
                (tl,[Strue])::rest, Strue

    | (Sconst v)::ss ->
if debug then Format.printf "peu: const %a\n%!" print_interval v;
            let zv = value_at_ tu false z in
            let v = if neg then (-$)v else v in
            let s = Sconst v in
            begin match compare_vs v zv with
            | 0 -> proc_evt_ut_ debug neg tl tu rest z ss
            | 1 -> proc_evt_ut_ debug neg tl tu rest s ss
            | _ -> proc_evt_ut_ debug neg tl tu rest z ss (* TODO *)
            end

    | (Sexpr (_lid,_apid,Rise) as s)::ss as ss0 ->
if debug then Format.printf "peu: expr rise %b %f\n%!" neg tl;
            let zvu = value_at_ tu false z in
            let  vu = value_at_ tu neg s in
            let res = compare_vs vu zvu in
(*Format.printf "res: %d\n%!" res;*)
            if neg then (* Fall *)
                begin match res with
                | 1 -> proc_evt_ut_ debug false tl tu rest (Snot s) ss
                | 0 -> 
                    let zvl = value_at_ tl false z in
                    let  vl = value_at_ tl true s in
                    let res = compare_vs vl zvl in
(*Format.printf "res: %d\n%!" res;*)
                    begin match res with
                    | 0 -> proc_evt_ut_ debug false tl tu rest z ss
                    | _ ->
                        let ctl,ctu = find_intersection_ true false s z tl tu in
(*Format.printf "cs_: %d, [%f,%f]\n%!" apid ctl ctu;*)
                        begin if ctl > ctu (* no intersection *) then 
                            proc_evt_ut_ debug false tl tu rest z ss
                        else
                            proc_evt_ut_ debug true tl ctl ((ctl,[z])::rest) (Snot s) ss0
                        end
                    end
                | _ -> proc_evt_ut_ debug false tl tu rest (Snot s) ss (* TODO *)
                end
            else (* Rise *)
                let s = Sconst vu in
                begin match res with
                | 0 -> proc_evt_ut_ debug false tl tu rest z ss
                | 1 -> proc_evt_ut_ debug false tl tu rest s ss
                | _ -> proc_evt_ut_ debug false tl tu rest z ss (* TODO *)
                end

    | ((Sexpr (_lid,_apid,Fall) as s)::ss as ss0) ->
if debug then Format.printf "peu: expr fall %b %f\n%!" neg tl;
            let zvu = value_at_ tu false z in
            let  vu = value_at_ tu neg s in
            let res = compare_vs vu zvu in
(*Format.printf "res: %d\n%!" res;*)
            begin if neg then (* Rise *)
                let s = Sconst vu in
                match res with
                | 0 -> proc_evt_ut_ debug false tl tu rest z ss
                | 1 -> proc_evt_ut_ debug false tl tu rest s ss
                | _ -> proc_evt_ut_ debug false tl tu rest z ss (* TODO *)

            else (* Fall *)
                match res with
                | 1 -> proc_evt_ut_ debug false tl tu rest s ss
                | 0 ->
                    let zvl = value_at_ tl false z in
                    let  vl = value_at_ tl neg s in
                    let res = compare_vs vl zvl in
(*Format.printf "res: %d\n%!" res;*)
                    begin match res with
                    | 0 -> proc_evt_ut_ debug false tl tu rest z ss
                    | _ ->
                        let ctl,ctu = find_intersection_ neg false s z tl tu in
(*Format.printf "peu_: [%f,%f]\n%!" ctl ctu;*)
                        if ctl > ctu (* no intersection *) then 
                            proc_evt_ut_ debug false tl tu rest z ss
                        else
                            proc_evt_ut_ debug false tl ctl ((ctl,[z])::rest) s ss0
                    end
                | _ -> proc_evt_ut_ debug false tl tu rest s ss (* TODO *)
            end

    | (Sexpr (_lid,_apid,Unknown) as s)::ss ->
if debug then Format.printf "peu: expr unk %b %f\n%!" neg tl;
            let zv = value_at_ tu false z in
            let  v = value_at_ tu neg s in
            begin match compare_vs v zv with
            | 0 -> proc_evt_ut_ debug false tl tu rest (Sconst zv) ss
            | 1 -> proc_evt_ut_ debug false tl tu rest (Sconst  v) ss
            | _ -> proc_evt_ut_ debug false tl tu rest (Sconst zv) ss (* TODO *)
            end

    | _::_ss ->
            assert false


let proc_evt_ut debug tmax (tl,ss) (rest,z) = 
    let tu = get_time_u tmax rest in
    proc_evt_ut_ debug false tl tu rest z ss


let nth_time fp n = fst (List.nth fp n)
let nth_sig  fp n = List.hd (snd (List.nth fp n)) (* TODO *)

let rec proc_evt_ l u t0 tmax s is i fp fp_ =
    let tmin = nth_time fp (Dlist.first !is) in
    let t_ = nth_time fp (!i+1) in
    let t = min (tmin -. l) (t_ -. u) in
    if t = tmin -. l then begin
        is := Dlist.remove_first !is;
        s := t
    end;
    if t = t_ -. u then begin
        let v1 = value_at_ (t_-.u) false (nth_sig fp (!i+1)) in
        let imax = Dlist.last !is in
        let v2 = value_at_ (nth_time fp imax) false (nth_sig fp imax) in
        (* TODO *)
        while compare_vs v1 v2 = 1 && not (Dlist.is_empty !is) do
            is := Dlist.remove_last !is
        done;
        is := Dlist.insert_last !is (!i+1);
        incr i
    end;
    let pre = 
        if s >= t0 then
            (* propagate _ _ Mor *)
            let v = value_at_ tmin false (nth_sig fp (Dlist.first !is)) in
            proc_or !s t fp_ [(!s,[Sconst v])]
        else
            []
    in
    List.append pre (if t+.l < tmax then proc_evt_ l u t0 tmax s is i fp fp_ else [])

let rec shift_sig st = function
    | Sexpr _ as s -> Sshift (st,s)
    | Sshift (st0,s) -> Sshift (st0+.st,s)
    | Snot s -> Snot (shift_sig st s)
    | _ as s -> s

let shift_pipe st tmax (t,ss) rest =
    let ts = t -. st in
    let tu = get_time_u tmax rest in
    (*let tu = tu -. st in*) (* TODO *)
    if ts < 0. && tu < 0. then 
        rest 
    else begin
        let ts = if ts < 0. then 0. else ts in
        (ts, List.map (shift_sig st) ss)::rest
    end
    (* TODO: should search the extremums after tmax *)

let proc_evt tl tu t0 tmax fp = 
    let fp_l = List.fold_right (shift_pipe tl tmax) fp [] in
    let fp_u = List.fold_right (shift_pipe tu tmax) fp [] in
    let fp_ = proc_or 0. tmax fp_l fp_u in
    let s = t0 -. tl in
    let is = Dlist.create () in
    proc_evt_ tl tu (ref 0.) tmax (ref s) (ref is) (ref 0) fp fp_


(* computation of STL robustness signal *)
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
        let fp = List.map (fun (t,ss) -> t, List.map proc_not ss) fp in
        if debug then Format.printf "  not\n%a" print_fp fp;
        fp
    | Mand (f1,f2) -> 
        let fp = proc_and 0. !time_max (propagate debug ap_fps f1) 
                                       (propagate debug ap_fps f2) in
        if debug then Format.printf "  and\n%a" print_fp fp;
        fp
    | Mor (f1,f2) -> 
        let fp = proc_or 0. !time_max (propagate debug ap_fps f1) 
                                      (propagate debug ap_fps f2) in
        if debug then Format.printf "  or\n%a" print_fp fp;
        fp
    | Mevt_ut f ->
        let fp = propagate debug ap_fps f in
        let fp,_ = List.fold_right (proc_evt_ut debug !time_max) fp ([],Sfalse) in
        if debug then Format.printf "  evt_ut\n%a" print_fp fp;
        fp
    (*| Muntil (t,f1,f2) -> 
        let bs1 = propagate debug ap_bs f1 in
        let bs2 = propagate debug ap_bs f2 in
        let bs  = shift_bs t bs1 bs2 in
        if debug then Format.printf "  until %a\n%a" print_interval t print_bs bs;
        bs
    *)
    | Mevt (t,f) ->
        let fp = propagate debug ap_fps f in
        let fp = proc_evt t.inf t.sup 0. !time_max fp in
        if debug then Format.printf "  evt %a\n%a" print_interval t print_fp fp;
        fp
    | _ ->
        error Unsupported


(* preserve the parameter values. *)
let param_values = Queue.create ()

let get_param_value (id,bnd) =
    id, (Random.float bnd)

let set_param_ lid (id,value) = 
    set_param lid id value


let simulate (ps,_var,(iloc,_ival),locs) (aps,_ap_locs) =
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

        (*let pvs = List.map get_param_value ps in
        Queue.add pvs param_values;
        let _ = List.map (set_param_ !curr_loc) pvs in*)

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
    (*print_pped true true;*)
    dispose ();
    (*Printf.printf "fpf: %d\n" !c_fpf;*)
    !ap_fps


let rec dump_signal is_neg st tl tu = function
    | Strue  -> dump_bool is_neg tl tu
    | Sfalse -> dump_bool (not is_neg) tl tu
    | Sconst v ->
            dump_const is_neg v.inf v.sup tl tu
    | Sexpr (lid,apid,_polar) -> 
            dump_ap lid apid is_neg st tl tu
    | Snot s -> 
            dump_signal (not is_neg) st tl tu s
    | Sshift (st1,s) ->
            dump_signal is_neg (st+.st1) tl tu s

let rec dump_signals tmax = function 
    | (tl,ss)::rest ->
      let tu = get_time_u tmax rest in
(*Printf.printf "dump_sig: %f %f\n" tl tu;*)
      let _ = List.map (dump_signal false 0. tl tu) ss in
      dump_signals tmax rest

    | [] -> ()

let dump_fp (_ps,_var,(iloc,_ival),_locs) fp (*(aps,ap_locs)*) =
    (* turn on the dump function. *)
    Capd_sending_stubs.set_solving_param "dump_to_file" 1.;

    let curr_loc = ref iloc in

    let pvs = Queue.take param_values in
    let _ = List.map (set_param_ !curr_loc) pvs in

    initialize ();

    dump_signals !time_max fp;

    (* TODO *)
    print_pped true true;
    dispose ();
    ()

