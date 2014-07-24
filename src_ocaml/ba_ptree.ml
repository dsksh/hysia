open Format
open Model_common
open Pretty
open Ptree

type ba_expr = loc * ba_expr_node
and  ba_expr_node =
  | Pvar of ident
  | Pbool of bool
  | Papp of un_op * ba_expr
  | Papp2 of bin_op * ba_expr * ba_expr

type id = ident
type edge = loc * (ba_expr * id)
type location = loc * (id * edge list)

type param = unit
type init  = unit
type dexpr = unit
type gexpr = ba_expr
type rexpr = unit

type t = location list

let rec print_expr fmt = function
  | _, Pvar id -> fprintf fmt "%s" id
  | _, Pbool b -> fprintf fmt "%b" b
  | _, Papp (op,e) -> 
      fprintf fmt "%s %a" (sprint_un_op op) print_expr e
  | _, Papp2 (op,e1,e2) -> 
      fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op) print_expr e2


let print_param fmt _ = fprintf fmt ""
let print_id fmt id = fprintf fmt "%s" id
let print_init fmt _ = fprintf fmt ""
let print_dexpr fmt _ = fprintf fmt ""
let print_gexpr fmt e = fprintf fmt "%a" print_expr e
let print_rexpr fmt _ = fprintf fmt ""

let id_of_loc      (_,(e,_)) = e
let dexprs_of_loc  (_,_) = []
let edges_of_loc   (_,(_,e)) = e
let gh_of_edge     (_,(e,_)) = e
let gg_of_edge     (_,_) = (dummy_loc, Pbool true)
let dst_of_edge    (_,(_,e)) = e 
let rexprs_of_edge (_,_) = []
