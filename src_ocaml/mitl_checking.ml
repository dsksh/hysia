open Model_common
open Model
open Util

(* Some [] : empty; None : universe *)

let invert_fs = function
    | None -> Some []
    | Some [] -> None
    | Some fs ->
            Some (List.map (fun (ap,p) -> ap, not p) fs)

let cmp_fs f1 f2 = match f1,f2 with
    | (Interval (l1,u1), _), (Interval (l2,u2), _) -> 
            int_of_float (if l1 <> l2 then l1-.l2 else u2-.u1)
    | _,_ -> 
            print_endline "1";
            assert false

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
                | _, _ -> 
                        print_endline "2";
                        assert false
            in
            let _,fs = List.fold_left sel (0,[]) fs in
            Some fs

let shift_fs tmax i fs = 
    match i with
    | Interval (il,iu) -> begin
        match fs with
        | None -> 
                None
        | Some [] -> 
                (*Some [(Interval (tmax-.il,tmax-.il),false)]*)
                Some []
        | Some fs ->
                let shift fs f = match f with
                    | Interval (tl,tu) as t, polar ->
                        let o = if polar then iu else il in
                        let tl,tu = tl-.o, tu-.o in
Printf.printf "shifted: %f %f\n" tl tu;
                        if tl >= 0. then
                            (Interval (tl,tu), polar)::fs
                        else if tu >= 0. then
                            (Interval (0.,tu), polar)::fs
                        else
                            (Interval (0.,0.), polar)::fs
                            (*fs*)
                    (*| Interval (tl,tu) as t, false ->
                        let tl,tu = tl-.il, tu-.il in
                        if tl >= 0. then
                            (Interval (tl,tu), false)::fs
                        else if tu >= 0. then
                            (Interval (0.,tu), false)::fs
                        else
                            fs
                    *)
                    | _,_ -> 
                            print_endline "3";
                            assert false
                in
                let i,p = List.hd fs in
                let fs = match i with
                  | Interval (0.,_) -> fs;
                  | Interval _ ->
                    (* status at time 0 should be expressed explicitly. *)
                    (Interval (0.,0.), not p)::fs
                in
                let fs1 = List.fold_left shift [] fs in
                if fs1 = [] then
                    (* result will be universe or empty *)
                    let _,p = List.nth fs (List.length fs -1) in
                    if p then Some [] else None
                else 
                    (* sort fs *)
                    let cmp (Interval (tl1,_),_) (Interval (tl2,_),_) =
                        int_of_float (tl1 -. tl2)
                    in
                    let fs = List.sort cmp fs1 in

                    let n_signals = ref 0 in
                    let remove_overlaps fs (Interval (tl,tu),polar as f) =
                        if polar then begin
                            incr n_signals;
                            if !n_signals = 1 && tu > 0. then List.append fs [f] else fs
                        end else begin
                            if !n_signals > 0 then decr n_signals;
                            if !n_signals = 0 && tu > 0. then List.append fs [f] else fs
                        end
                    in
                    let fs = List.fold_left remove_overlaps [] fs in
                    Some fs 
    end
    | _ -> 
           print_endline "4";
           assert false



let print_fs fmt = function
    | Some [] -> Format.fprintf fmt "universe\n"
    | None -> Format.fprintf fmt "empty\n"
    | Some fs ->
        let pr = function
        | Model_common.Interval (l,u), p -> Format.fprintf fmt "[%f;%f] %b\n" l u p;
        | _ -> 
               print_endline "5";
               assert false
        in
        List.map pr fs; ()
    | _ -> ()

let rec check tmax ap_fs (*ap_locs*) = function
    | Mtrue -> Printf.printf "true\n"; Some []
    | Mloc (id,lid) ->
        (*let i = ref (-1) in
        let _ = mapi (fun i_ lid_ -> if lid_ = lid then i := i_) ap_locs in*)
        let fs = snd (List.nth ap_fs id) in
        Format.printf "loc\n%a" print_fs fs;
        if fs = Some [] then None else fs
    | Mexpr d -> 
        let fs = snd (List.find (fun (apid,fs) -> apid = d.tag) ap_fs) in
        Format.printf "expr\n%a" print_fs fs;
        fs
    | Mnot f -> 
        let fs = invert_fs (check tmax ap_fs f) in
        Format.printf "not\n%a" print_fs fs;
        fs
    | Mand (f1,f2) -> 
        let fs = intersect_fs (check tmax ap_fs f1) (check tmax ap_fs f2) in
        Format.printf "and\n%a" print_fs fs;
        fs
    | Muntil (i,f1,f2) -> 
        let fs1 = check tmax ap_fs f1 in
        let fs2 = check tmax ap_fs f2 in
        (*let fs = intersect_fs (shift_fs tmax i (intersect_fs fs1 fs2)) fs1 in*)
        let fs = intersect_fs fs1 fs2 in
        Format.printf "until00 %a\n%a" Model_common.print_interval i print_fs fs;
        let fs = shift_fs tmax i fs in
        Format.printf "until01 %a\n%a" Model_common.print_interval i print_fs fs;
        let fs = intersect_fs fs fs1 in
        Format.printf "until %a\n%a" Model_common.print_interval i print_fs fs;
        fs

let eval_at_zero = function
    | None -> Some false
    | Some [] -> Some true 
    | Some fs -> let Model_common.Interval (l,u), p = List.nth fs 0 in
        if l > 0. then begin
            if p then Some false else Some true
        end else None

