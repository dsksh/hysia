open Model
open Interval
open Util

(* Some [] : empty; None : universe *)

let print_fs fmt = function
    | Some [] -> Format.fprintf fmt "    empty\n"
    | None    -> Format.fprintf fmt "    universe\n"
    | Some fs ->
        let pr (t,p) = Format.fprintf fmt "    %a %b\n" print_interval t p in
        let _ = List.map pr fs in 
        ()

(*(* TODO *)
let int_of_float_ex v = int_of_float (100000000.*.v)*)

let cmp_fs (t1, _) (t2, _) =
    (*int_of_float_ex (if t1.inf <> t2.inf then t1.inf-.t2.inf else t2.sup-.t1.sup)*)
    if t1.inf <> t2.inf then compare t1.inf t2.inf else compare t1.sup t2.sup

let normalize_fs fs = 
    (* sort fs *)
    (*let cmp (t1,_) (t2,_) = int_of_float_ex (t1.inf -. t2.inf) in*)
    let fs = List.sort cmp_fs fs in

    let if_contains_zero = function
        | (t,false) ->
                (*t.inf < t.sup && Interval.intersect t Interval.zero <> None*)
                t.inf <= 0. && 0. < t.sup
        | (_t,true) -> 
                false
    in
    let check_if_contains_zero fs =
        try 
            let _ = List.find if_contains_zero fs in
            error UnknownOverlap
        with
        | Not_found -> ()
    in
    let _ = check_if_contains_zero fs in

    let n_overlaps = ref 0 in
    let rec filter_embedded = function
        | (_,true as f)::fs -> 
                incr n_overlaps;
                if !n_overlaps >= 2 then
                    (* f is included in another interval in fs *)
                    filter_embedded fs
                else begin
                    f::(filter_embedded fs)
                end
        | (_,false as f)::fs -> 
                decr n_overlaps;
                if !n_overlaps >= 1 then
                    (* f is included in another interval in fs *)
                    filter_embedded fs
                else begin
                    f::(filter_embedded fs)
                end
        | [] -> []
    in
    let fs = filter_embedded fs in

    let filter_negative = function
        | (t,true as f)::[] -> if t.sup > 0. then f::fs else [(Interval.zero,true)]
        | (t,_ as f)::fs    -> if t.sup > 0. then f::fs else fs
        | [] -> []
    in
    let fs = filter_negative fs in

    let normalize_overlap_opp (t,polar) f (res,fs) = 
        match f with
        | (t1,polar1 as f) when polar <> polar1 -> 
            begin match intersect t t1 with
            | Some _t -> 
                    (*if t.inf = t.sup then 
                        (* remove the bound *)
                        (true, fs)
                    else*)
                    error UnknownOverlap
            | None   -> res, f::fs
            end
        | _ -> res, f::fs
    in
    let normalize_overlap_homo (t,polar) f (res,fs) = 
        match f with
        | (t1,polar1 as f) when polar = polar1 -> 
            begin match intersect t t1 with
            | Some _ -> true, (join t t1, polar)::fs
            | None   -> res,  f::fs
            end
        | f -> res, f::fs
    in
    let rec normalize_overlaps = function
        | f::fs -> 
            let _,fs = List.fold_right (normalize_overlap_opp  f) fs (false,[]) in
            let r,fs = List.fold_right (normalize_overlap_homo f) fs (false,[]) in
            if r then 
                (* f is merged with another frontier in fs *)
                normalize_overlaps fs
            else 
                f::(normalize_overlaps fs)
        | [] -> [] 
    in
    let fs = normalize_overlaps fs in

    let fs = match fs with
        | (_t,false as f)::fs -> (Interval.zero,true)::(f::fs)
        | _ -> fs
    in

    Some fs

    (*let rec remove_overlaps = function
        | (t,true as f)::fs -> 
            let r,fs = List.fold_right (check_overlap_opp f) fs (false,[]) in
            if r then (* TODO: should not reach here *)
                (* f is merged with another frontier *)
                remove_overlaps fs
            else begin
                incr n_overlaps;
                if !n_overlaps >= 2 then
                    (* f is included in another interval in fs *)
                    remove_overlaps fs
                else begin
                    let r,fs = List.fold_right (check_overlap_homo f) fs (false,[]) in
                    if r then 
                        (* f is merged with another frontier in fs *)
                        remove_overlaps fs
                    else 
                        (*incr n_overlaps;*)
                        f::(remove_overlaps fs)
                end
            end
        | (t,false as f)::fs -> 
            decr n_overlaps;
            let r,fs = List.fold_right (check_overlap_homo f) fs (false,[]) in
            if r then 
                (* f is merged with another frontier in fs *)
                remove_overlaps fs
            else begin
                if !n_overlaps >= 1 then
                    (* f is included in another interval in fs *)
                    remove_overlaps fs
                else begin
                    let r,fs = List.fold_right (check_overlap_opp f) fs (false,[]) in
                    if r then 
                        (* f is merged with another frontier *)
                        remove_overlaps fs
                    else begin
                        decr n_overlaps;
                        f::(remove_overlaps fs)
                    end
                end
            end
        | [] -> []
    in
    Some (remove_overlaps fs)
    *)


let invert_fs = function
    | None -> Some []
    | Some [] -> None
    | Some fs ->
            let fs = List.map (fun (ap,p) -> ap, not p) fs in

            (* normalize *)
            match fs with
            | (t,_false as e)::fs ->
                if 
                    (*t.inf < t.sup && Interval.intersect t Interval.zero <> None*) 
                    t.inf <= 0. && 0. < t.sup
                then
                    error UnknownOverlap
                else if (*t = Interval.zero*) t.sup = 0. then
                    Some fs
                else if t.inf > 0. then
                    Some ((Interval.zero, true)::(e::fs))
                else (* t should not be strictly negative *)
                    assert false
                    (*Some fs*)
                    (*Some (({inf=t.inf; sup=t.inf}, true)::(f::fs))*)
            | [] (* this doesn't match *) -> 
                    assert false

let join_fs fs1 fs2 = match fs1, fs2 with
    | None, _ -> None
    | _, None -> None
    | Some [], fs2 -> fs2
    | fs1, Some [] -> fs1
    | Some fs1, Some fs2 ->
            let fs = List.merge cmp_fs fs1 fs2 in
            normalize_fs fs

let intersect_fs fs1 fs2 = match fs1, fs2 with
    | Some [], _ -> Some []
    | _, Some [] -> Some []
    | None, fs2 -> fs2
    | fs1, None -> fs1
    | Some fs1, Some fs2 ->
            let fs1 = invert_fs (Some fs1) in
(*Format.printf "\n%a" print_fs (Some fs1);*)
            let fs2 = invert_fs (Some fs2) in
            let fs1,fs2 = match fs1,fs2 with
            | Some fs1, Some fs2 -> fs1,fs2
            | _,_ -> assert false
            in
(*Format.printf "\n%a" print_fs (Some fs2);*)
            let fs = List.merge cmp_fs fs1 fs2 in
            let fs = normalize_fs fs in
            invert_fs fs

(*let shift_fs tmax t fs = 
    match fs with
    | None -> 
            None
    | Some fs ->
            let shift f fs = 
                let s, polar = f in
                let o = if polar then t.sup else t.inf in
                let s1 = s -$. o in
(*Printf.printf "shifted: [%f, %f] - %f = [%f, %f] %b\n" s.inf s.sup o s1.inf s1.sup polar;*)
                (s1, polar)::fs
            in
            let fs = List.fold_right shift fs [] in

            normalize_fs fs
*)

let shift_elem t bs = 
    let shift (u,polar) = 
        let o = if polar then t.sup else t.inf in
        let u1 = u -$. o in
(*Printf.printf "shifted: [%f, %f] - %f = [%f, %f] %b\n" u.inf u.sup o u1.inf u1.sup polar;*)
        u1,polar
    in
    let bs = List.map shift bs in
    normalize_fs bs

let rec map_pairs proc = function
    | (_,true as b1)::(_,false as b2)::bs ->
            let bs1 = proc [b1;b2] in
            let bs2 = map_pairs proc bs in
            let bs1,bs2 = match bs1,bs2 with
            | Some bs1, Some bs2 -> bs1,bs2
            | _ -> assert false
            in
            Some (List.append bs1 bs2)
    | (_,true as b1)::[] ->
            proc [b1]
    | [] -> Some []
    | _  -> assert false

let shift_fs t fs1 fs2 = match fs1, fs2 with
    | Some [], _ -> Some []
    | _, Some [] -> Some []
    | None, None -> None
    | None, Some fs2 ->
            map_pairs (shift_elem t) fs2
    | Some fs1, None ->
            let proc bs = 
                let bs1 = shift_elem t bs in
                intersect_fs (Some bs) bs1
            in
            map_pairs proc fs1
    | Some fs1, Some fs2 ->
            let proc1 bs1 bs2 =
                let bs = match intersect_fs (Some bs1) (Some bs2) with
                | Some bs -> bs
                | _None -> assert false
                in
                let bs = shift_elem t bs in
                intersect_fs bs (Some bs1)
            in
            let proc2 bs1 = 
                map_pairs (proc1 bs1) fs2
            in
            map_pairs proc2 fs1


let rec mod_intervals debug tmax ap_fs (*ap_locs*) = function
    | Mtrue -> if debug then Printf.printf "  true\n"; None
    | Mloc (id,_lid) ->
        let fs = snd (List.nth ap_fs id) in
        if debug then Format.printf "  loc\n%a" print_fs fs;
        if fs = Some [] then None else fs
    | Mexpr d -> 
        let fs = snd (List.find (fun (apid,_fs) -> apid = d.Hashcons.tag) ap_fs) in
        let fs = match fs with
        | Some [(t,true)] when t.sup = 0. -> None;
        | fs -> fs;
        in
        if debug then Format.printf "  expr %d\n%a" d.Hashcons.tag print_fs fs;
        fs
    | Mnot f -> 
        let fs = invert_fs (mod_intervals debug tmax ap_fs f) in
        if debug then Format.printf "  not\n%a" print_fs fs;
        fs
    | Mor (f1,f2) -> 
        let fs = join_fs (mod_intervals debug tmax ap_fs f1) 
                         (mod_intervals debug tmax ap_fs f2) in
        if debug then Format.printf "  or\n%a" print_fs fs;
        fs
    | Muntil (i,f1,f2) -> 
        let fs1 = mod_intervals debug tmax ap_fs f1 in
        let fs2 = mod_intervals debug tmax ap_fs f2 in
        (*let fs = intersect_fs fs1 fs2 in
        if debug then Format.printf "  until(1) %a\n%a" print_interval i print_fs fs;
        let fs = shift_fs tmax i fs in
        if debug then Format.printf "  until(2) %a\n%a" print_interval i print_fs fs;
        let fs = intersect_fs fs fs1 in
        if debug then Format.printf "  until(3) %a\n%a" print_interval i print_fs fs;
        *)
        let fs = shift_fs i fs1 fs2 in
        if debug then Format.printf "  until %a\n%a" print_interval i print_fs fs;
        fs

let eval_at_zero = function
    | None -> Some true
    | Some [] -> Some false
    | Some fs -> 
        (*let t, p = List.nth fs 0 in 
        if t.inf > 0. then begin if p then Some false else Some true end else None*)

        let count_before_zero c (t,_) =
            if t.sup < 0. then begin
                (*if polar then incr n_overlaps
                else decr_pos n_overlaps;*)
                c+1
            end
            else c
        in
        let c = List.fold_left count_before_zero 0 fs in

        let t, p = List.nth fs c in 
        if t.inf > 0. then begin 
            if p then Some false else Some true 
        end else if t.sup = 0. then begin 
            if p then Some true else Some false 
        end else None
