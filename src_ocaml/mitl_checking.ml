open Model
open Interval
open Util

(* Some [] : empty; None : universe *)

let print_bs fmt = function
    | Some [] -> Format.fprintf fmt "    empty\n"
    | None    -> Format.fprintf fmt "    universe\n"
    | Some bs ->
        let pr (u,p) = Format.fprintf fmt "    %a %b\n" print_interval u p in
        let _ = List.map pr bs in 
        ()

let cmp_bs (u1,_) (u2,_) =
    if u1.inf <> u2.inf then compare u1.inf u2.inf else compare u1.sup u2.sup

let normalize_bs bs = 
    (* sort bs *)
    let bs = List.sort cmp_bs bs in

    let if_contains_zero = function
        | (u,false) ->
                (*u.inf < u.sup && Interval.intersect u Interval.zero <> None*)
                u.inf <= 0. && 0. < u.sup
        | (_,true) -> false
    in
    let check_if_contains_zero bs =
        try 
            let _ = List.find if_contains_zero bs in
            error UnknownOverlap
        with
        | Not_found -> ()
    in
    let _ = check_if_contains_zero bs in

    let normalize_overlap_opp (u,polar) b (res,bs) = 
        match b with
        | (u1,polar1 as b) when polar <> polar1 -> 
            begin match intersect u u1 with
            | Some _u -> 
                    error UnknownOverlap
            | None   -> res, b::bs
            end
        | _ -> res, b::bs
    in
    let normalize_overlap_homo (u,polar) b (res,bs) = 
        match b with
        | (u1,polar1 as b) when polar = polar1 -> 
            begin match intersect u u1 with
            | Some _ -> true, (join u u1, polar)::bs
            | None   -> res,  b::bs
            end
        | b -> res, b::bs
    in
    let rec normalize_overlaps = function
        | b::bs -> 
            let _,bs = List.fold_right (normalize_overlap_opp  b) bs (false,[]) in
            let r,bs = List.fold_right (normalize_overlap_homo b) bs (false,[]) in
            if r then 
                (* b is merged with another frontier in bs *)
                normalize_overlaps bs
            else 
                b::(normalize_overlaps bs)
        | [] -> [] 
    in
    (* apply for detecting the undecidable overlaps *)
    let _ = normalize_overlaps bs in

    let n_overlaps = ref 0 in
    let rec filter_embedded = function
        | (_,true as f)::bs -> 
                incr n_overlaps;
                if !n_overlaps >= 2 then
                    (* f is included in another interval in bs *)
                    filter_embedded bs
                else begin
                    f::(filter_embedded bs)
                end
        | (_,false as f)::bs -> 
                decr n_overlaps;
                if !n_overlaps >= 1 then
                    (* f is included in another interval in bs *)
                    filter_embedded bs
                else begin
                    f::(filter_embedded bs)
                end
        | [] -> []
    in
    let bs = filter_embedded bs in
(*let _ = Format.printf "  fe %a\n" print_bs (Some bs) in*)

    let filter_negative = function
        | (u,true as b)::[] -> if u.sup > 0. then b::bs else [(Interval.zero,true)]
        | (u,_ as b)::bs    -> if u.sup > 0. then b::bs else bs
        | [] -> []
    in
    let bs = filter_negative bs in

    (*let normalize_overlap_opp (u,polar) b (res,bs) = 
        match b with
        | (u1,polar1 as b) when polar <> polar1 -> 
            begin match intersect u u1 with
            | Some _u -> 
                    (*if u.inf = u.sup then 
                        (* remove the bound *)
                        (true, bs)
                    else*)
                    error UnknownOverlap
            | None   -> res, b::bs
            end
        | _ -> res, b::bs
    in
    let normalize_overlap_homo (u,polar) b (res,bs) = 
        match b with
        | (u1,polar1 as b) when polar = polar1 -> 
            begin match intersect u u1 with
            | Some _ -> true, (join u u1, polar)::bs
            | None   -> res,  b::bs
            end
        | b -> res, b::bs
    in
    let rec normalize_overlaps = function
        | b::bs -> 
            let _,bs = List.fold_right (normalize_overlap_opp  b) bs (false,[]) in
            let r,bs = List.fold_right (normalize_overlap_homo b) bs (false,[]) in
            if r then 
                (* f is merged with another frontier in bs *)
                normalize_overlaps bs
            else 
                b::(normalize_overlaps bs)
        | [] -> [] 
    in*)
    (* apply again for removing the overlappings *)
    let bs = normalize_overlaps bs in

    let bs = match bs with
        | (_u,false as b)::bs -> (Interval.zero,true)::(b::bs)
        | _ -> bs
    in

    Some bs


let invert_bs = function
    | None -> Some []
    | Some [] -> None
    | Some bs ->
            let bs = List.map (fun (ap,p) -> ap, not p) bs in

            (* normalize *)
            match bs with
            | (u,_false as b)::bs ->
                if 
                    (*u.inf < u.sup && Interval.intersect u Interval.zero <> None*) 
                    u.inf <= 0. && 0. < u.sup
                then
                    error UnknownOverlap
                else if (*u = Interval.zero*) u.sup = 0. then
                    Some bs
                else if u.inf > 0. then
                    Some ((Interval.zero, true)::(b::bs))
                else (* t should not be strictly negative *)
                    assert false
            | [] (* this doesn't match *) -> 
                    assert false

let join_bs bs1 bs2 = match bs1, bs2 with
    | None, _ -> None
    | _, None -> None
    | Some [], bs2 -> bs2
    | bs1, Some [] -> bs1
    | Some bs1, Some bs2 ->
            let bs = List.merge cmp_bs bs1 bs2 in
            normalize_bs bs

let intersect_bs bs1 bs2 = match bs1, bs2 with
    | Some [], _ -> Some []
    | _, Some [] -> Some []
    | None, bs2 -> bs2
    | bs1, None -> bs1
    | Some bs1, Some bs2 ->
            let bs1 = invert_bs (Some bs1) in
(*Format.printf "\n%a" print_bs (Some bs1);*)
            let bs2 = invert_bs (Some bs2) in
            let bs1,bs2 = match bs1,bs2 with
            | Some bs1, Some bs2 -> bs1,bs2
            | _,_ -> assert false
            in
(*Format.printf "\n%a" print_bs (Some bs2);*)
            let bs = List.merge cmp_bs bs1 bs2 in
            let bs = normalize_bs bs in
            invert_bs bs


let shift_elem t bs = 
    let shift (u,polar) = 
        let o = if polar then t.sup else t.inf in
        let u1 = u -$. o in
(*Printf.printf "shifted: [%f, %f] - %f = [%f, %f] %b\n" u.inf u.sup o u1.inf u1.sup polar;*)
        u1,polar
    in
    let bs = List.map shift bs in
    match normalize_bs bs with
    | Some bs -> bs
    | _ -> assert false

let rec map_pairs proc = function
    | (_,true as b1)::(_,false as b2)::bs ->
            let bs1 = proc [b1;b2] in
            let bs2 = map_pairs proc bs in
            (*let bs1,bs2 = match bs1,bs2 with
            | Some bs1, Some bs2 -> bs1,bs2
            | _ -> assert false
            in*)
            List.append bs1 bs2
    | (_,true as b1)::[] ->
            proc [b1]
    | [] -> []
    | _  -> assert false

let shift_bs t bs1 bs2 = match bs1, bs2 with
    | Some [], _ -> Some []
    | _, Some [] -> Some []
    | None, None -> None
    | None, Some bs2 ->
            normalize_bs (map_pairs (shift_elem t) bs2)
    | Some bs1, None ->
            let proc bs = 
                let bs1 = shift_elem t bs in
                match intersect_bs (Some bs) (Some bs1) with
                | Some bs -> bs
                | _ -> assert false
            in
            normalize_bs (map_pairs proc bs1)
    | Some bs1, Some bs2 ->
(*let _ = Format.printf "  shift_bs %a\n%a" print_bs (Some bs1) print_bs (Some bs2) in*)
            let proc1 bs1 bs2 =
(*let _ = Format.printf "  proc %a\n%a\n" print_bs (Some bs1) print_bs (Some bs2) in*)
                let bs = match intersect_bs (Some bs1) (Some bs2) with
                | Some bs -> bs
                | _None -> assert false
                in
                let bs = shift_elem t bs in
                match intersect_bs (Some bs) (Some bs1) with
                | Some bs -> 
(*let _ = Format.printf "  res: %a" print_bs (Some bs) in*)
                    bs
                | _ -> assert false
            in
            let proc2 bs1 = 
                map_pairs (proc1 bs1) bs2
            in
            normalize_bs (map_pairs proc2 bs1)


let rec propagate debug ap_bs = function
    | Mtrue -> if debug then Printf.printf "  true\n"; None
    | Mloc (id,_lid) ->
        let bs = snd (List.nth ap_bs id) in
        if debug then Format.printf "  loc\n%a" print_bs bs;
        (*if bs = Some [] then None else bs*)
        bs
    | Mexpr d -> 
        let bs = snd (List.find (fun (apid,_bs) -> apid = d.Hashcons.tag) ap_bs) in
        let bs = match bs with
        | Some [(u,true)] when u.sup = 0. -> None;
        | bs -> bs;
        in
        if debug then Format.printf "  expr %d\n%a" d.Hashcons.tag print_bs bs;
        bs
    | Mnot f -> 
        let bs = invert_bs (propagate debug ap_bs f) in
        if debug then Format.printf "  not\n%a" print_bs bs;
        bs
    | Mor (f1,f2) -> 
        let bs = join_bs (propagate debug ap_bs f1) 
                         (propagate debug ap_bs f2) in
        if debug then Format.printf "  or\n%a" print_bs bs;
        bs
    | Muntil (t,f1,f2) -> 
        let bs1 = propagate debug ap_bs f1 in
        let bs2 = propagate debug ap_bs f2 in
        let bs  = shift_bs t bs1 bs2 in
        if debug then Format.printf "  until %a\n%a" print_interval t print_bs bs;
        bs
    | Mevt (t,f) -> 
        let bs = propagate debug ap_bs f in
        let bs = shift_bs t None bs in
        if debug then Format.printf "  evt %a\n%a" print_interval t print_bs bs;
        bs
    | _ ->
        error Unsupported

let eval_at_zero = function
    | None -> Some true
    | Some [] -> Some false
    | Some bs -> 
        (*let t, p = List.nth bs 0 in 
        if t.inf > 0. then begin if p then Some false else Some true end else None*)

        let count_before_zero c (u,_) =
            if u.sup < 0. then begin
                (*if polar then incr n_overlaps
                else decr_pos n_overlaps;*)
                c+1
            end
            else c
        in
        let c = List.fold_left count_before_zero 0 bs in

        let u,p = List.nth bs c in 
        if u.inf > 0. then begin 
            if p then Some false else Some true 
        end else if u.sup = 0. then begin 
            if p then Some true else Some false 
        end else None
