open Format
open Model_common
open Pretty

type loc = Lexing.position * Lexing.position

type var = loc * ident

type expr = loc * expr_node
and  expr_node =
  | Pvar of ident
  | Pint of int
  | Pval of interval
  | Papp of un_op * expr
  | Papp2 of bin_op * expr * expr

type var_l = loc * (var list)
type expr_l = loc * (expr list)
type interval_l = loc * (interval list)

type der   = expr
type init  = (*interval*) expr
type grd   = expr
type jump  = expr
type param = loc * (string * interval)

type t = var_l * expr_l * (*interval_l*) expr_l * expr * expr * expr_l * param list

let simplify ((_,var),(_,der),(_,init),grd_h,grd_g,(_,jump),ps) =
  (var,der,init,grd_h,grd_g,jump,ps)


let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
let dummy_list = dummy_loc, []
let dummy_grd  = dummy_loc, Pval (Point (-1.))


let rec print_expr fmt = function
  | _, Pvar id -> fprintf fmt "%s" id
  | _, Pint v  -> fprintf fmt "%d" v
  | _, Pval (Point v) -> fprintf fmt "%f" v
  | _, Pval (Interval (l,u)) -> fprintf fmt "[%f;%f]" l u
  | _, Papp (op,e) -> 
      fprintf fmt "%s %a" (sprint_un_op op) print_expr e
  | _, Papp2 (op,e1,e2) -> 
      fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op) print_expr e2


(*module P : Pretty.Printer =
struct
  type var = lvar
  type der = lexpr
  type init = interval
  type grd = lexpr
  type jump = lexpr
  type param = string * interval

  let print_var fmt (_,id) = fprintf fmt "%s" id
  let print_der fmt e = fprintf fmt "%a" print_expr e
  let print_init fmt v = fprintf fmt "%a" print_interval v
  let print_grd fmt e = fprintf fmt "%a" print_expr e
  let print_jump fmt e = fprintf fmt "%a" print_expr e
  let print_param fmt (id,v) = fprintf fmt "%s:=%a" id print_interval v
end
*)

let print_var fmt (_,id) = fprintf fmt "%s" id
let print_der fmt e = fprintf fmt "%a" print_expr e
let print_init fmt v = fprintf fmt "%a" (*print_interval*) print_expr v
let print_grd fmt e = fprintf fmt "%a" print_expr e
let print_jump fmt e = fprintf fmt "%a" print_expr e
let print_param fmt (_,(id,v)) = fprintf fmt "%s:=%a" id print_interval v

(*module Printer = Pretty.Make(P)*)
