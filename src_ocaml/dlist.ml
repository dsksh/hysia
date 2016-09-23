type 'a elem = 
{
    value : 'a;
    mutable prev : 'a t;
    mutable next : 'a t
}
and 
'a t = 'a elem option

(* 3.2 Circular link list *)

let create () = None

let is_empty t = t = None

let insert_first t v = 
    match t with 
    | None -> 
        let e = { value = v; prev = None; next = None } in
        let t = Some e in
        e.prev <- t;
        e.next <- t;
        t
    | Some fst as t0 ->
        let e = { value = v; prev = None; next = None } in
        let t = Some e in
        let e_last = fst.prev in
        begin match e_last with
        | None -> ()
        | Some e -> e.next <- t
        end;
        e.next <- t0;
        e.prev <- fst.prev;
        fst.prev <- t;
        t

let insert_last t v = 
    match t with
    | None -> insert_first None v
    | Some fst as t0 ->
        let e_last = 
            match fst.prev with
            | None -> failwith "Incorrect state"
            | Some e -> e
        in
        let e = { value = v; prev = None; next = None } in
        let t = Some e in
        e_last.next <- t;
        e.prev <- fst.prev ;
        fst.prev <- t;
        t0

let first_elem = function
    | None -> failwith "Empty list"
    | Some e -> e

let last_elem t = first_elem (first_elem t).prev

let first t = (first_elem t).value
let last t = (last_elem t).value

let remove_first t =
    let e_fst = first_elem t in
    let e_last = last_elem t in
    let t = e_fst.next in
    if e_fst.prev = None && e_fst.next = None then
        None
    else begin
        e_fst.prev <- Some e_last;
        e_last.next <- t;
        t
    end

let remove_last t =
    let e_fst = first_elem t in
    let e_last = last_elem t in
    let e_last1 = first_elem e_last.prev in
    let t = Some e_fst in
    e_last1.next <- t;
    e_fst.prev <- Some e_last1;
    t
