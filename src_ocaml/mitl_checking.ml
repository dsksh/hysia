open Model
open Interval

(* Some [] : empty; None : universe *)

let invert_fs = function
    | None -> Some []
    | Some [] -> None
    | Some fs ->
            Some (List.map (fun (ap,p) -> ap, not p) fs)

let cmp_fs (t1, _) (t2, _) =
    int_of_float (if t1.inf <> t2.inf then t1.inf-.t2.inf else t2.sup-.t1.sup)

let intersect_fs fs1 fs2 = match fs1, fs2 with
    | None, _ -> None
    | _, None -> None
    | Some [], fs2 -> fs2
    | fs1, Some [] -> fs1
    | Some fs1, Some fs2 ->
            let fs = List.merge cmp_fs fs1 fs2 in
            let sel (s,res) = function
                | t, true -> 
                        if s = 1 then (2, List.append res [(t,true)]) else (1,res)
                | t, false -> 
                        if s = 2 then (1, List.append res [(t,false)]) else (0,res)
            in
            let _,fs = List.fold_left sel (0,[]) fs in
            Some fs

let shift_fs tmax t fs = 
    match fs with
    | None -> 
            None
    | Some [] -> 
            (*Some [({inf=tmax-.v.inf; sup=tmax-.v.inf},false)]*)
            Some []
    | Some fs ->
            let shift fs f = 
                let t1, polar = f in
                if t1.inf <= tmax then begin
                    let o = if polar then t.sup else t.inf in
                    let tl,tu = t1.inf-.o, t1.sup-.o in
(*Printf.printf "shifted: %f %f\n" tl tu;*)
                    if tl >= 0. then
                        ({inf=tl;sup=tu}, polar)::fs
                    else if tu >= 0. then
                        ({inf=0.;sup=tu}, polar)::fs
                    else
                        ({inf=0.;sup=0.}, polar)::fs
                        (*fs*)
                end else fs
            in
            let t0,p = List.hd fs in
            let fs = match t0 with
              | {inf=0.} -> fs;
              | _ ->
                (* status at time 0 should be expressed explicitly. *)
                ({inf=0.;sup=0.}, not p)::fs
            in
            let fs1 = List.fold_left shift [] fs in
            if fs1 = [] then
                (* result will be universe or empty *)
                let _,p = List.nth fs (List.length fs -1) in
                if p then Some [] else None
            else 
                (* sort fs *)
                let cmp (t1,_) (t2,_) = int_of_float (t1.inf -. t2.inf) in
                let fs = List.sort cmp fs1 in

                let n_signals = ref 0 in
                let remove_overlaps fs (t,polar as f) =
                    if polar then begin
                        incr n_signals;
                        if !n_signals = 1 && t.sup > 0. then List.append fs [f] else fs
                    end else begin
                        if !n_signals > 0 then decr n_signals;
                        if !n_signals = 0 && t.sup > 0. then List.append fs [f] else fs
                    end
                in
                let fs = List.fold_left remove_overlaps [] fs in
                Some fs 



let print_fs fmt = function
    | Some [] -> Format.fprintf fmt "universe\n"
    | None -> Format.fprintf fmt "empty\n"
    | Some fs ->
        let pr (t,p) = Format.fprintf fmt "%a %b\n" print_interval t p in
        let _ = List.map pr fs in 
        ()

let rec check debug tmax ap_fs (*ap_locs*) = function
    | Mtrue -> if debug then Printf.printf "true\n"; Some []
    | Mloc (id,_lid) ->
        (*let i = ref (-1) in
        let _ = mapi (fun i_ lid_ -> if lid_ = lid then i := i_) ap_locs in*)
        let fs = snd (List.nth ap_fs id) in
        if debug then Format.printf "loc\n%a" print_fs fs;
        if fs = Some [] then None else fs
    | Mexpr d -> 
        let fs = snd (List.find (fun (apid,_fs) -> apid = d.Hashcons.tag) ap_fs) in
        if debug then Format.printf "expr %d\n%a" d.Hashcons.tag print_fs fs;
        fs
    | Mnot f -> 
        let fs = invert_fs (check debug tmax ap_fs f) in
        if debug then Format.printf "not\n%a" print_fs fs;
        fs
    | Mand (f1,f2) -> 
        let fs = intersect_fs (check debug tmax ap_fs f1) (check debug tmax ap_fs f2) in
        if debug then Format.printf "and\n%a" print_fs fs;
        fs
    | Muntil (i,f1,f2) -> 
        let fs1 = check debug tmax ap_fs f1 in
        let fs2 = check debug tmax ap_fs f2 in
        (*let fs = intersect_fs (shift_fs tmax i (intersect_fs fs1 fs2)) fs1 in*)
        let fs = intersect_fs fs1 fs2 in
        if debug then Format.printf "until00 %a\n%a" print_interval i print_fs fs;
        let fs = shift_fs tmax i fs in
        if debug then Format.printf "until01 %a\n%a" print_interval i print_fs fs;
        let fs = intersect_fs fs fs1 in
        if debug then Format.printf "until %a\n%a" print_interval i print_fs fs;
        fs

let eval_at_zero = function
    | None -> Some false
    | Some [] -> Some true 
    | Some fs -> 
        let t, p = List.nth fs 0 in 
        if t.inf > 0. then begin if p then Some false else Some true end else None
