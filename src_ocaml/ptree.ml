open Format
open Model_common
open Pretty

type loc = Lexing.position * Lexing.position

type pval = PVint of Interval.t | PVrandom of float
type param = loc * (string * pval)

type id = loc * ident
type lid = loc * ident

type expr = loc * expr_node
and  expr_node =
  | Pvar of ident
  | Pint of int
  | Pval of Interval.t
  | Papp of un_op * expr
  | Papp2 of bin_op * expr * expr

type mitl_formula = loc * mitl_node
and  mitl_node =
  | Ptrue
  | Ploc of ident
  | Pexpr of expr
  | Pnot of mitl_node
  | Pand of mitl_node * mitl_node
  | Por  of mitl_node * mitl_node
  (*| Pimp of mitl_node * mitl_node*)
  | Puntil of Interval.t * mitl_node * mitl_node
  | Palw of Interval.t * mitl_node
  | Pevt of Interval.t * mitl_node
  | Puntil_ut of mitl_node * mitl_node
  | Palw_ut of mitl_node
  | Pevt_ut of mitl_node

type id_l = loc * (id list)

type expr_l = loc * (expr list)
type interval_l = loc * (Interval.t list)

type edge = loc * (bool * expr * expr_l * lid * expr_l)
type edge_l = loc * (edge list)
type location = loc * (lid * expr_l * expr_l * edge_l)

type t = (param list * id_l * expr_l * location list) * mitl_formula

type init = expr list
type dexpr = expr
type iexpr = expr
type gexpr = expr 
type rexpr = expr
type prop  = mitl_formula

let simplify (params,(_,vars),(_,init),locs) =
  (params,vars,init,locs)


let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
let dummy_list = dummy_loc, []
let dummy_grd  = dummy_loc, Pval (Interval.interval_of_float (-1.))
let dummy_prop = dummy_loc, Ptrue


let rec print_expr fmt = function
  | _, Pvar id -> fprintf fmt "%s" id
  | _, Pint v  -> fprintf fmt "%d" v
  (*| _, Pval (Point v) -> fprintf fmt "%f" v*)
  | _, Pval v -> fprintf fmt "%a" Interval.print_interval v
  (*| _, Pval Empty -> fprintf fmt "(empty)"*)
  | _, Papp (op,e) -> 
      fprintf fmt "%s %a" (sprint_un_op op) print_expr e
  | _, Papp2 (op,e1,e2) -> 
      fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op) print_expr e2

let rec print_prop fmt (_,p) = print_prop_node fmt p
and print_prop_node fmt = function
  | Ptrue -> fprintf fmt "true"
  | Ploc id -> fprintf fmt "L[%s]" id
  | Pexpr e -> fprintf fmt "%a" print_expr e
  | Pnot p -> fprintf fmt "!%a" print_prop_node p
  | Pand (p1,p2) -> fprintf fmt "(%a /\\ %a)" print_prop_node p1 print_prop_node p2
  | Por (p1,p2) -> fprintf fmt "(%a \\/ %a)" print_prop_node p1 print_prop_node p2
  (*| Pimp (p1,p2) -> fprintf fmt "(%a -> %a)" print_prop_node p1 print_prop_node p2*)
  | Puntil (v,p1,p2) -> fprintf fmt "(%a U%a %a)"
     print_prop_node p1 Interval.print_interval v print_prop_node p2
  | Palw (v,p) -> fprintf fmt "(G%a %a)" Interval.print_interval v print_prop_node p
  | Pevt (v,p) -> fprintf fmt "(F%a %a)" Interval.print_interval v print_prop_node p
  | Puntil_ut (p1,p2) -> fprintf fmt "(%a U %a)" print_prop_node p1 print_prop_node p2
  | Palw_ut p -> fprintf fmt "(G %a)" print_prop_node p
  | Pevt_ut p -> fprintf fmt "(F %a)" print_prop_node p

let print_param fmt (_,(id,v)) = match v with
  | PVint v -> fprintf fmt "%s:=%a" id Interval.print_interval v
  | PVrandom bnd -> fprintf fmt "%s:=R(%f)" id bnd
let print_id fmt (_,id) = fprintf fmt "%s" id
let print_init fmt e = fprintf fmt "%a" (print_list "," print_expr) e
let print_dexpr fmt e = fprintf fmt "%a" print_expr e
let print_iexpr fmt e = fprintf fmt "%a" print_expr e
let print_gexpr fmt e = fprintf fmt "%a" print_expr e
let print_rexpr fmt e = fprintf fmt "%a" print_expr e

let id_of_loc      (_,(e,_,_,_)) = e
let dexprs_of_loc  (_,(_,e,_,_)) = snd e
let iexprs_of_loc  (_,(_,_,e,_)) = snd e
let edges_of_loc   (_,(_,_,_,e)) = snd e
let gf_of_edge     (_,(e,_,_,_,_)) = e
let gh_of_edge     (_,(_,e,_,_,_)) = e
let gg_of_edge     (_,(_,_,e,_,_)) = snd e
let dst_of_edge    (_,(_,_,_,e,_)) = e 
let rexprs_of_edge (_,(_,_,_,_,e)) = snd e 
