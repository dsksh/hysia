open Model
open Interval
open Util

(* Some [] : empty; None : universe *)

let print_fs fmt = function
    | Some [] -> Format.fprintf fmt "    empty\n"
    | None -> Format.fprintf fmt "    universe\n"
    | Some fs ->
        let pr (t,p) = Format.fprintf fmt "    %a %b\n" print_interval t p in
        let _ = List.map pr fs in 
        ()

let invert_fs = function
    | None -> Some []
    | Some [] -> None
    | Some fs ->
            let fs = List.map (fun (ap,p) -> ap, not p) fs in
            match fs with
            | (t,_ as f)::fs ->
                if t.sup > 0. then
                    let fs = ({inf=(max 0. t.inf); sup=(max 0. t.sup)}, false)::fs in
                    Some (({inf=0.; sup=0.}, true)::fs)
                else
                    Some fs
                    (*Some (({inf=t.inf; sup=t.inf}, true)::(f::fs))*)
            | [] (* this doesn't match *) -> 
                    Some []

let cmp_fs (t1, _) (t2, _) =
    int_of_float (if t1.inf <> t2.inf then t1.inf-.t2.inf else t2.sup-.t1.sup)

let merge_fs fs = 
    (* sort fs *)
    let cmp (t1,_) (t2,_) = int_of_float (100000000.*.(t1.inf -. t2.inf)) (*TODO*) in
    let fs = List.sort cmp fs in

    let n_overlaps = ref 0 in

    (*let decr_pos = if !n_overlaps > 0 then decr n_overlaps in
    let remove_lt_zero fs (t,polar as f) =
        if t.sup >= 0. then f::fs
        else if polar then incr n_overlaps
        else decr_pos n_overlaps
    in
    let fs = List.fold_left remove_lt_zero [] fs in*)

    let check_overlap_opp (t,polar) f res = 
        match f with
        | (t1,polar1 as f) when polar <> polar1 -> 
            begin match intersect t t1 with
            | Some _ -> true
            | None   -> res
            end
        | _ -> res
    in
    let check_overlap_homo (t,polar) f (res,fs) = 
        match f with
        | (t1,polar1 as f) when polar = polar1 -> 
            begin match intersect t t1 with
            | Some _ -> true, (union t t1, polar)::fs
            | None   -> res,  f::fs
            end
        | f -> (res, f::fs)
    in
    let rec remove_overlaps = function
        | (t,true as f)::fs -> 
            begin if List.fold_right (check_overlap_opp f) fs false then
                error UnknownOverlap
            end;
            incr n_overlaps;
            if !n_overlaps >= 2 then
                (* included in another interval *)
                remove_overlaps fs
            else begin
                let r,fs = List.fold_right (check_overlap_homo f) fs (false,[]) in
                if r then 
                    (* merged with another frontier *)
                    remove_overlaps fs
                else begin
                    (*incr n_overlaps;*)
                    f::(remove_overlaps fs)
                end
            end
        | (t,false as f)::fs -> 
            decr n_overlaps;
            let r,fs = List.fold_right (check_overlap_homo f) fs (false,[]) in
            if r then 
                (* merged with another frontier *)
                remove_overlaps fs
            else begin
                if !n_overlaps >= 1 then
                    (* included in another interval *)
                    remove_overlaps fs
                else begin
                    if List.fold_right (check_overlap_opp f) fs false then
                        error UnknownOverlap
                    else begin
                        decr n_overlaps;
                        f::(remove_overlaps fs)
                    end
                end
            end
        | [] -> []
    in
    Some (remove_overlaps fs)

let intersect_fs fs1 fs2 = match fs1, fs2 with
    | Some [], _ -> Some []
    | _, Some [] -> Some []
    | None, fs2 -> fs2
    | fs1, None -> fs1
    | Some fs1, Some fs2 ->
            (*let fs = List.merge cmp_fs fs1 fs2 in
            let sel (s,res) = function
                | t, true -> 
                        if s = 1 then (2, List.append res [(t,true)]) else (1,res)
                | t, false -> 
                        if s = 2 then (1, List.append res [(t,false)]) else (0,res)
            in
            let _,fs = List.fold_left sel (0,[]) fs in
            Some fs*)

            let Some fs1 = invert_fs (Some fs1) in
Format.printf "\n%a" print_fs (Some fs1);
            let Some fs2 = invert_fs (Some fs2) in
Format.printf "\n%a" print_fs (Some fs2);
            let fs = List.merge cmp_fs fs1 fs2 in
            let fs = merge_fs fs in
            invert_fs fs

let shift_fs tmax t fs = 
    match fs with
    | None -> 
            None
    (*| Some [] -> 
            (*Some [({inf=tmax-.v.inf; sup=tmax-.v.inf},false)]*)
            Some []*)
    | Some fs ->
            (*let shift fs f = 
                let s, polar = f in
                if s.inf <= tmax then begin
                    let o = if polar then t.sup else t.inf in
                    (*let tl,tu = s.inf -. o, s.sup -. o in*)
                    let s = s -$. o in
(*Printf.printf "shifted: %f %f %b\n" s.inf s.sup polar;*)
                    if s.inf >= 0. then
                        (s, polar)::fs
                    else if s.sup >= 0. then
                        ({inf=0.;sup=s.sup}, polar)::fs
                    else
                        (Interval.zero, polar)::fs
                        (*fs*)
                end else fs
            in*)
            let shift f fs = 
                let s, polar = f in
                let o = if polar then t.sup else t.inf in
                let s1 = s -$. o in
Printf.printf "shifted: [%f, %f] - %f = [%f, %f] %b\n" s.inf s.sup o s1.inf s1.sup polar;
                (s1, polar)::fs
            in
            (*let t0,p = List.hd fs in
            let fs = match t0 with
              | {inf=0.} -> fs;
              | _ ->
                if not p then
                  (* status at time 0 should be expressed explicitly. *)
                  (Interval.zero, not p (* correct? *))::fs
                else fs
            in*)
            let fs = List.fold_right shift fs [] in

            merge_fs fs


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
    | Mand (f1,f2) -> 
        let fs = intersect_fs (mod_intervals debug tmax ap_fs f1) 
                              (mod_intervals debug tmax ap_fs f2) in
        if debug then Format.printf "  and\n%a" print_fs fs;
        fs
    | Muntil (i,f1,f2) -> 
        let fs1 = mod_intervals debug tmax ap_fs f1 in
        let fs2 = mod_intervals debug tmax ap_fs f2 in
        (*let fs = intersect_fs (shift_fs tmax i (intersect_fs fs1 fs2)) fs1 in*)
        let fs = intersect_fs fs1 fs2 in
        if debug then Format.printf "  until00 %a\n%a" print_interval i print_fs fs;
        let fs = shift_fs tmax i fs in
        if debug then Format.printf "  until01 %a\n%a" print_interval i print_fs fs;
        let fs = intersect_fs fs fs1 in
        if debug then Format.printf "  until %a\n%a" print_interval i print_fs fs;
        fs

let eval_at_zero = function
    | None -> Some true
    | Some [] -> Some false
    | Some fs -> 
        (*let t, p = List.nth fs 0 in 
        if t.inf > 0. then begin if p then Some false else Some true end else None*)

        let count_before_zero c (t,polar as f) =
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
