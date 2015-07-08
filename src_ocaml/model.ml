
open Format
open Hashcons
open Model_common
open Interval
open Ptree
open Pretty

type expr = expr_node hash_consed
and  expr_node =
  | Var of ident
  (*| Int of int*)
  | Val of Interval.t
  | App of un_op * expr
  | App2 of bin_op * expr * expr

module Expr_node = struct
  type t = expr_node

  let equal e1 e2 = e1 = e2
    (*match e1, e2 with
    | Pvar n1, Pvar n2 -> n1 = n2
    | Pint v1, Pint v2 -> v1 = v2
    | Pval v1, Pval v2 -> v1 = v2
    | Papp (op1,e1), Papp (op2,e2) -> op1=op2 && equal e1 e2
    | Papp2 (op1,e1,f2), Papp (op2,e2,f2) -> op1=op2 && equal e1 e2 && equal f1 f2
    | _ -> false
    *)

  (* TODO *)
  let hash = function
    | Var n -> Hashtbl.hash n
    (*| Val (Point v) -> int_of_float v*)
    | Val v -> int_of_float (v.inf+.v.sup) 
    | Val _ -> assert false
    | App (op,e) -> 19*e.tag + (match op with
        | Osqr -> 1 | Osqrt -> 2 | Oexp -> 3| Olog -> 4 | Osin -> 5 | Ocos -> 6
        | Oatan -> 7 | Oasin -> 8 | Oacos -> 9 | Oneg -> 10)
    | App2 (op,e1,e2) -> 19*(e1.tag + e2.tag) + (match op with
        | Oadd -> 11 | Osub -> 12 | Omul -> 13 | Odiv -> 14 | Opow -> 15 
        | Oand -> 16 | Oor -> 17)
  (*let hash_max = ref 0
  let hash _ = incr hash_max; !hash_max
  *)
end

module Hexpr = Make_consed(Expr_node)
module PMap = Map.Make(String)

let mk_var id        = Hexpr.hashcons (Var id)
let mk_val v         = Hexpr.hashcons (Val v)
let mk_app op e      = match op,e.node with
  | (Osqr|Osqrt|Osin),Val z when z = Interval.zero -> e
  | (Oexp|Ocos),Val z when z = Interval.zero -> 
        Hexpr.hashcons (Val (Interval.interval_of_float 1.))
  | (Osqr|Osqrt),Val {inf=1.;sup=1.} -> e
  | Olog,Val {inf=1.;sup=1.} -> 
        Hexpr.hashcons (Val Interval.zero)
  | _ -> Hexpr.hashcons (App (op,e))
let mk_app2 op e1 e2 = match op,e1.node,e2.node with
  | Oadd,Val z,_  when z = Interval.zero -> e2
  | (Oadd|Osub),_,Val z when z = Interval.zero -> e1
  | Odiv,_,Val z when z = Interval.zero -> assert false
  | (Omul|Odiv),Val z,_ when z = Interval.zero -> 
        Hexpr.hashcons (Val Interval.zero)
  | Omul,_,Val z when z = Interval.zero -> 
        Hexpr.hashcons (Val Interval.zero)
  | Omul,Val z,_ when z = Interval.one -> e2
  | (Omul|Odiv),_,Val z when z = Interval.one -> e1
  | _ -> Hexpr.hashcons (App2 (op,e1,e2))

let rec mk_expr pm = function
  | _, Pvar id     -> 
          if PMap.mem id pm then mk_val (PMap.find id pm) else mk_var id
  | _, Pint v      -> mk_val (Interval.interval_of_float (float_of_int v))
  | _, Pval v      -> mk_val v
  | _, Papp (op,e) -> mk_app op (mk_expr pm e)
  | _, Papp2 (op,e1,e2) -> mk_app2 op (mk_expr pm e1) (mk_expr pm e2)


type dual = dual_node hash_consed
and  dual_node = expr * expr list

module Dual_node = struct
  type t = dual_node

  let equal e1 e2 = fst e1 = fst e2

  let hash e = (fst e).tag
end

module Hdual = Make_consed(Dual_node)

let rec diff_expr vid expr = 
  let diff = diff_expr vid in
  match expr.node with
    | Var id -> if id = vid then mk_val Interval.one else mk_val Interval.zero
    | Val _ -> mk_val Interval.zero

    | App (Osqr,e) -> 
        (*(mk_app2 Omul (mk_val (Point 2.)) (mk_app2 Omul e (mk_app Osqr (diff e))))*)
        (mk_app2 Omul (diff e) (mk_app2 Omul (mk_val (interval_of_float 2.)) e))
    | App (Osqrt,e) -> 
        (mk_app2 Odiv (diff e) (mk_app2 Omul (mk_val (interval_of_float 2.)) (mk_app Osqrt e)))
    | App (Oexp,e) -> 
        (mk_app2 Omul (mk_app Oexp e) (diff e))
    | App (Olog,e) -> 
        (mk_app2 Odiv (diff e) e)
    | App (Osin,e) -> 
        (mk_app2 Omul (mk_app Ocos e) (diff e))
    | App (Ocos,e) -> 
        (mk_app2 Omul (mk_app2 Osub (mk_val Interval.zero) (mk_app Osin e)) (diff e))
    | App (Oatan,e) -> 
        (mk_app2 Omul (mk_app2 Odiv (mk_val Interval.one) (mk_app2 Oadd (mk_val Interval.one) (mk_app Osqr e))) (diff e))
    | App _ -> assert false

    | App2 (Oadd,e1,e2) -> 
        (mk_app2 Oadd (diff e1) (diff e2))
    | App2 (Osub,e1,e2) -> 
        (mk_app2 Osub (diff e1) (diff e2))
    | App2 (Omul,e1,e2) -> 
        (mk_app2 Oadd (mk_app2 Omul (diff e1) e2) (mk_app2 Omul e1 (diff e2)))
    | App2 (Odiv,e1,e2) -> 
        (mk_app2 Odiv (mk_app2 Osub (mk_app2 Omul (diff e1) e2) (mk_app2 Omul e1 (diff e2))) (mk_app Osqr e2))
    | App2 (Opow,e,n) -> 
        if n = mk_val (Interval.interval_of_float 3.) then
          (mk_app2 Omul (diff e) (mk_app2 Omul n (mk_app Osqr e)))
        else
          (mk_app2 Omul (diff e) (mk_app2 Omul n (mk_app2 Opow e (mk_app2 Osub n (mk_val Interval.one)))))
    | App2 _ -> assert false

let mk_dual var e =
  let de = List.map (fun v -> diff_expr v e) var in
  Hdual.hashcons (e,de)

let mk_dual_expr pm var expr = 
    mk_dual var (mk_expr pm expr)

(* construct the normal vector i.e. Dx fun . der. *)
let mk_normal var der dual =
  let mk_term e0 (de,der) =
    let (der_e,_) = der.node in
    let e = mk_app2 Omul de der_e in
      mk_app2 Oadd e0 e
  in
  let (_,de) = dual.node in
  let e0 = mk_val Interval.zero in
  let es = List.combine de der in
    mk_dual var (List.fold_left mk_term e0 es)

(* dummy *)
(*let mk_normal var der dual =
  mk_dual var (mk_val (Point (-1.)))*)

let mk_edge pm var der (_,(forced,grd_h,(_,grd_g),(_,dst),(_,jmp))) =
  let grd_h = mk_dual_expr pm var grd_h in
  let grd_g = List.map (mk_dual_expr pm var) grd_g in
  (*let gh_norm = mk_normal var der grd_h in
  let grd_g = gh_norm::grd_g in*)
  let jmp = List.map (mk_dual_expr pm var) jmp in
  (forced,grd_h,grd_g,dst,jmp)

let mk_loc pm var aps (_,((_,id),(_,der),(_,inv),(_,edges))) =
  let der = List.map (mk_dual_expr pm var) der in
  let inv = List.map (mk_dual_expr pm var) inv in
  let inorm = List.map (mk_normal var der) inv in
  let inv = List.combine inv inorm in
  let edges = List.map (mk_edge pm var der) edges in
  let aps = snd (List.split aps) in
  let ap_norms = List.map (fun ap -> mk_normal var der ap) aps in
  (id,der,inv,edges,aps,ap_norms)

let get_lid = function 
  | (_,Pvar lid) -> lid
  | _ -> assert false


(*module APMap = Map.Make(Int32)*)

type mitl_formula =
  | Mtrue
  | Mloc of int * ident
  | Mexpr of dual
  | Mnot of mitl_formula
  | Mand of mitl_formula * mitl_formula
  | Muntil of Interval.t * mitl_formula * mitl_formula

let rec mk_mitl_formula pm var aps ap_locs = function
  | Ptrue -> aps, ap_locs, Mtrue, 0.
  | Ploc lid ->
       let id = List.length ap_locs in
       aps, List.append ap_locs [lid], Mloc (id,lid), 0.
  | Pexpr e -> 
       let d = mk_dual_expr pm var e in
       (*APMap.add (Int32.of_int d.tag) d aps, Mexpr d*)
       List.append aps [(d.tag, d)], ap_locs, Mexpr d, 0.
  | Pnot (Pnot p) -> 
       let aps,ap_locs,p,l = mk_mitl_formula pm var aps ap_locs p in
       aps, ap_locs, p, l
  | Pnot p -> 
       let aps,ap_locs,p,l = mk_mitl_formula pm var aps ap_locs p in
       aps, ap_locs, Mnot p, l
  | Pand (p1,p2) -> 
       let aps,ap_locs,p1,l1 = mk_mitl_formula pm var aps ap_locs p1 in
       let aps,ap_locs,p2,l2 = mk_mitl_formula pm var aps ap_locs p2 in
       aps, ap_locs, Mand (p1,p2), max l1 l2
  | Puntil (t,p1,p2) -> 
       let aps,ap_locs,p1,l1 = mk_mitl_formula pm var aps ap_locs p1 in
       let aps,ap_locs,p2,l2 = mk_mitl_formula pm var aps ap_locs p2 in
       aps, ap_locs, Muntil (t,p1,p2), (max l1 l2) +. t.sup
  | Puntil _ -> assert false


let make (ps,var,iloc::ival,locs) prop = 
  (*let nv,nd = List.length var, List.length der in
  if nv <> nd then error (DimMismatch (nv,nd)) loc
  else*)
  let ps  = List.map snd ps in
  let add_param (ps,pm) (id,v) = match v with
    | PVint v -> (ps, PMap.add id v pm)
    | PVrandom bnd -> ((id,bnd)::ps, pm)
  in
  let ps,pm = List.fold_left add_param ([],PMap.empty) ps in

  let var = List.map snd var in

  (*let aps = APMap.empty in*)
  let aps,ap_locs,prop,len = mk_mitl_formula pm var [] [] (snd prop) in

  let iloc = get_lid iloc in
  let ival = List.map (mk_expr pm) ival in
  let locs = List.map (mk_loc pm var aps) locs in

  (ps,var,(iloc,ival),locs), (aps,ap_locs,prop,len)

(* for testing *)
let make_prop var prop = 
  (*let ps  = List.map snd ps in*)
  (*let add_param (ps,pm) (id,v) = match v with
    | PVint v -> (ps, PMap.add id v pm)
    | PVrandom bnd -> ((id,bnd)::ps, pm)
  in
  let ps,pm = List.fold_left add_param ([],PMap.empty) ps in*)

  mk_mitl_formula PMap.empty var [] [] (snd prop)

type param = string * float
type id = ident
type init = ident * expr list
type dexpr = dual
type iexpr = dual * dual
type gexpr = dual
type rexpr = dual
type prop  = mitl_formula
type edge = bool * gexpr * gexpr list * ident * rexpr list
type location = ident * dexpr list * iexpr list * edge list * dual list * dual list

let rec print_expr fmt expr = match expr.node with
  | Var id -> fprintf fmt "%s" id
  (*| Val (Point v) -> fprintf fmt "%f" v*)
  | Val v  -> fprintf fmt "%a" print_interval v
  | Val _ -> assert false
  | App (op,e) -> 
      fprintf fmt "%s %a" (sprint_un_op op) print_expr e
  | App2 (op,e1,e2) -> 
      fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op)
      print_expr e2

let print_dual fmt dual = 
  let (e,d) = dual.node in
  fprintf fmt "@[(%a(%d)@ [%a])@]" print_expr e dual.tag (print_list "," print_expr) d

let print_param fmt (id,bnd) = fprintf fmt "%s:=R(%f)" id bnd
let print_id fmt id = fprintf fmt "%s" id
let print_init fmt (iloc,iexpr) = fprintf fmt "%s %a" iloc (print_list "," print_expr) iexpr
let print_dexpr fmt e = fprintf fmt "%a" print_dual e
let print_iexpr fmt (e,ne) = fprintf fmt "(%a,@ [%a]" print_dual e print_dual ne
let print_gexpr fmt e = fprintf fmt "%a" print_dual e
let print_rexpr fmt e = fprintf fmt "%a" print_dual e

let rec print_prop fmt = function
  | Mtrue -> fprintf fmt "true"
  | Mloc (id,lid) -> fprintf fmt "L[%s]" lid
  | Mexpr d -> fprintf fmt "%a" print_dual d
  | Mnot p -> fprintf fmt "!%a" print_prop p
  | Mand (p1,p2) -> fprintf fmt "(%a & %a)" print_prop p1 print_prop p2
  | Muntil (v,p1,p2) -> fprintf fmt "%a U%a %a"
                                     print_prop p1 print_interval v print_prop p2
  | Muntil (_,p1,p2) -> ()

let id_of_loc      (e,_,_,_,_,_) = e
let dexprs_of_loc  (_,e,_,_,_,_) = e
let iexprs_of_loc  (_,_,e,_,_,_) = e
let edges_of_loc   (_,_,_,e,_,_) = e
let gh_of_edge     (_,e,_,_,_) = e
let gg_of_edge     (_,_,e,_,_) = e
let dst_of_edge    (_,_,_,e,_) = e 
let rexprs_of_edge (_,_,_,_,e) = e 
