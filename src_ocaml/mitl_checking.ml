open Model_common
open Model

(* Some [] : empty; None : universe *)

let invert_fs = function
    | None -> Some []
    | Some [] -> None
    | Some fs ->
            Some (List.map (fun (ap,p) -> ap, not p) fs)

let cmp_fs f1 f2 = match f1,f2 with
    | (Interval (l1,u1), _), (Interval (l2,u2), _) -> 
            int_of_float (if l1 <> l2 then l1-.l2 else u2-.u1)
    | _,_ -> assert false

let intersect_fs fs1 fs2 = match fs1, fs2 with
    | Some [], _ -> Some []
    | _, Some [] -> Some []
    | None, fs2 -> fs2
    | fs1, None -> fs1
    | Some fs1, Some fs2 ->
            let fs = List.merge cmp_fs fs1 fs2 in
            let sel (s,res) = function
                | t, true -> 
                        if s = 1 then (2, List.append res [(t,true)]) else (1,res)
                | t, false -> 
                        if s = 2 then (1, List.append res [(t,false)]) else (0,res)
                | _, _ -> assert false
            in
            let _,fs = List.fold_left sel (0,[]) fs in
            Some fs

let shift_fs tmax i fs = 
    match i with
    | Interval (il,iu) -> begin
        match fs with
        | None -> None
        | Some [] -> (*Some [(Interval (tmax-.l,tmax-.l),false)]*)
                     Some []
        | Some fs ->
                let shift f fs = match f with
                    | Interval (tl,tu) as t, true ->
                        let tl,tu = tl-.iu, tu-.iu in
                        if tl >= 0. then
                            (Interval (tl,tu), true)::fs
                        else if tu >= 0. then
                            (Interval (0.,tu), true)::fs
                        else 
                            fs
                    | Interval (tl,tu) as t, false ->
                        let tl,tu = tl-.il, tu-.il in
                        if tl >= 0. then
                            (Interval (tl,tu), false)::fs
                        else if tu >= 0. then
                            (Interval (0.,tu), false)::fs
                        else
                            fs
                    | _,_ -> assert false
                in
                let res = List.fold_right shift fs [] in
                if res = [] then
                    (* result will be universe or empty *)
                    let _,p = List.nth fs (List.length fs -1) in
                    if p then None else Some []
                else Some fs
    end
    | _ -> assert false


let print_fs fmt = function
    | Some [] -> Format.fprintf fmt "empty\n"
    | None -> Format.fprintf fmt "universe\n"
    | Some fs ->
        let pr = function
        | Model_common.Interval (l,u),p -> Format.fprintf fmt "[%f;%f] %b\n" l u p;
        | _ -> assert false
        in
        List.map pr fs; ()
    | _ -> ()

let rec check tmax ap_fs = function
    | Mtrue -> Printf.printf "true\n"; None
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
