type loc = Lexing.position * Lexing.position

type interval = 
  | Interval of float * float 
  | Point of float

type un_ops =
  | Osqr | Osqrt | Oexp | Olog | Osin | Ocos | Oatan | Oasin | Oacos

type bin_ops =
  | Oadd | Osub | Omul | Odiv | Opow

type var = loc * string

type rational = loc * (int * int)

type expr = loc * expr_node
and  expr_node =
  | Pvar of string
  | Pint of int
  | Pval of float
  | Papp of un_ops * expr
  | Papp2 of bin_ops * expr * expr

type vars = loc * (var list)

type def = expr

type def_vec = loc * (def list)

type init = loc * (int * interval list)

type param = loc * (string * interval)

module SM = Map.Make(String)
type model = (loc * (vars * def_vec * init * def * def * def_vec * param list)) * float SM.t


let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let dummy_vec = dummy_loc, []
let dummy_grd  = dummy_loc, Pval (-1.)
let dummy_init = dummy_loc, (0,[])
