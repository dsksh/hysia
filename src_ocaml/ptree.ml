open Format
open Model_common
open Pretty

type loc = Lexing.position * Lexing.position

type param = loc * (string * interval)

type id = loc * ident
type lid = loc * ident

type expr = loc * expr_node
and  expr_node =
  | Pvar of ident
  | Pint of int
  | Pval of interval
  | Papp of un_op * expr
  | Papp2 of bin_op * expr * expr

type id_l = loc * (id list)
type expr_l = loc * (expr list)
type interval_l = loc * (interval list)

type edge = loc * (expr * expr_l * lid * expr_l)
type edge_l = loc * (edge list)
type location = loc * (lid * expr_l * edge_l)

type t = param list * id_l * expr_l * location list

type init = expr list
type dexpr = expr
type gexpr = expr
type rexpr = expr

let simplify (params,(_,vars),(_,init),locs) =
  (params,vars,init,locs)


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


let print_param fmt (_,(id,v)) = fprintf fmt "%s:=%a" id print_interval v
let print_id fmt (_,id) = fprintf fmt "%s" id
let print_init fmt e = fprintf fmt "%a" (print_list "," print_expr) e
let print_dexpr fmt e = fprintf fmt "%a" print_expr e
let print_gexpr fmt e = fprintf fmt "%a" print_expr e
let print_rexpr fmt e = fprintf fmt "%a" print_expr e

let id_of_loc      (_,(e,_,_)) = e
let dexprs_of_loc  (_,(_,e,_)) = snd e
let edges_of_loc   (_,(_,_,e)) = snd e
let gh_of_edge     (_,(e,_,_,_)) = e
let gg_of_edge     (_,(_,e,_,_)) = snd e
let dst_of_edge    (_,(_,_,e,_)) = e 
let rexprs_of_edge (_,(_,_,_,e)) = snd e 
