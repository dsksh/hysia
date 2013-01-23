
open Format
open Hashcons
open Model_common
open Ptree
open Pretty

type expr = expr_node hash_consed
and  expr_node =
  | Var of ident
  (*| Int of int*)
  | Val of interval
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
    | Val (Point v) -> int_of_float v
    | Val (Interval (l,u)) -> int_of_float (l+.u) 
    | App (op,e) -> 19*e.tag + (match op with
        | Osqr -> 1 | Osqrt -> 2 | Oexp -> 3| Olog -> 4 | Osin -> 5 | Ocos -> 6
        | Oatan -> 7 | Oasin -> 8 | Oacos -> 9)
    | App2 (op,e1,e2) -> 19*(e1.tag + e2.tag) + (match op with
        | Oadd -> 1 | Osub -> 2 | Omul -> 3 | Odiv -> 4 | Opow -> 5)
end

module Hexpr = Make_consed(Expr_node)

let mk_var id        = Hexpr.hashcons (Var id)
let mk_val v         = Hexpr.hashcons (Val v)
let mk_app op e      = match op,e.node with
  | (Osqr|Osqrt|Osin),Val (Point 0.) -> e
  | (Oexp|Ocos),Val (Point 0.) -> Hexpr.hashcons (Val (Point 1.))
  | (Osqr|Osqrt),Val (Point 1.) -> e
  | Olog,Val (Point 1.) -> Hexpr.hashcons (Val (Point 0.))
  | _ -> Hexpr.hashcons (App (op,e))
let mk_app2 op e1 e2 = match op,e1.node,e2.node with
  | Oadd,Val (Point 0.),_ -> e2
  | (Oadd|Osub),_,Val (Point 0.) -> e1
  | Odiv,_,Val (Point 0.) -> assert false
  | (Omul|Odiv),Val (Point 0.),_ -> Hexpr.hashcons (Val (Point 0.))
  | Omul,_,Val (Point 0.) -> Hexpr.hashcons (Val (Point 0.))
  | Omul,Val (Point 1.),_ -> e2
  | (Omul|Odiv),_,Val (Point 1.) -> e1
  | _ -> Hexpr.hashcons (App2 (op,e1,e2))

let rec mk_expr = function
  | _, Pvar id     -> mk_var id
  | _, Pint v      -> mk_val (Point (float_of_int v))
  | _, Pval v      -> mk_val v
  | _, Papp (op,e) -> mk_app op (mk_expr e)
  | _, Papp2 (op,e1,e2) -> mk_app2 op (mk_expr e1) (mk_expr e2)


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
    | Var id -> if id = vid then mk_val (Point 1.) else mk_val (Point 0.)
    | Val _ -> mk_val (Point 0.)

    | App (Osqr,e) -> 
        (mk_app2 Omul (mk_val (Point 2.)) (mk_app2 Omul e (mk_app Osqr (diff e))))
    | App (Osqrt,e) -> 
        (mk_app2 Odiv (diff e) (mk_app2 Omul (mk_val (Point 2.)) (mk_app Osqrt e)))
    | App (Oexp,e) -> 
        (mk_app2 Omul (mk_app Oexp e) (diff e))
    | App (Olog,e) -> 
        (mk_app2 Odiv (diff e) e)
    | App (Osin,e) -> 
        (mk_app2 Omul (mk_app Ocos e) (diff e))
    | App (Ocos,e) -> 
        (mk_app2 Omul (mk_app2 Osub (mk_val (Point 0.)) (mk_app Osin e)) (diff e))
    | App (Oatan,e) -> 
        (mk_app2 Omul (mk_app2 Odiv (mk_val (Point 1.)) (mk_app2 Oadd (mk_val (Point 1.)) (mk_app Osqr e))) (diff e))
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
        if n = mk_val (Point 3.) then
          (mk_app2 Omul (diff e) (mk_app2 Omul n (mk_app Osqr e)))
        else
          (mk_app2 Omul (diff e) (mk_app2 Omul n (mk_app2 Opow e (mk_app2 Osub n (mk_val (Point 1.))))))

let mk_dual var e =
  let de = List.map (fun v -> diff_expr v e) var in
  Hdual.hashcons (e,de)

let mk_dexpr var = function
  | _, Pvar id     -> mk_dual var (mk_var id)
  | _, Pint v      -> mk_dual var (mk_val (Point (float_of_int v)))
  | _, Pval v      -> mk_dual var (mk_val v)
  | _, Papp (op,e) -> mk_dual var (mk_app op (mk_expr e))
  | _, Papp2 (op,e1,e2) -> mk_dual var (mk_app2 op (mk_expr e1) (mk_expr e2))

let make (var,der,init,grd_h,grd_g,jmp,ps) = 
  (*let nv,nd = List.length var, List.length der in
  if nv <> nd then error (DimMismatch (nv,nd)) loc
  else*)
  let var = List.map snd var in
  let der = List.map (mk_dexpr var) der in
  let init = List.map mk_expr init in
  let grd_h = mk_dexpr var grd_h in
  let grd_g = mk_dexpr var grd_g in
  let jmp = List.map (mk_dexpr var) jmp in
  let ps  = List.map snd ps in
  (var,der,init,grd_h,grd_g,jmp,ps)
    

type var   = ident
type der   = dual
type init  = (*interval*) expr
type grd   = dual
type jump  = dual
type param = string * interval

let rec print_expr fmt expr = match expr.node with
  | Var id -> fprintf fmt "%s" id
  | Val (Point v) -> fprintf fmt "%f" v
  | Val (Interval (l,u))  -> fprintf fmt "[%f;%f]" l u
  | App (op,e) -> 
      fprintf fmt "%s %a" (sprint_un_op op) print_expr e
  | App2 (op,e1,e2) -> 
      fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op)
      print_expr e2

let print_dual fmt dual = 
  let (e,d) = dual.node in
  fprintf fmt "@[(%a,@ [%a])@]" print_expr e (print_list "," print_expr) d

let print_var fmt id = fprintf fmt "%s" id
let print_der fmt e = fprintf fmt "%a" print_dual e
let print_init fmt v = fprintf fmt "%a" print_expr v
let print_grd fmt e = fprintf fmt "%a" print_dual e
let print_jump fmt e = fprintf fmt "%a" print_dual e
let print_param fmt (id,v) = fprintf fmt "%s:=%a" id print_interval v
