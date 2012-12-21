
type loc = Lexing.position * Lexing.position
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let dummy_vf = dummy_loc, ([],[])
let dummy_iv = dummy_loc, (0,[])

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

type vf = loc * (var list * expr list)

type iv = loc * (int * float list)

type param = loc * (string * float)

module SM = Map.Make(String)
type model = (loc * (vf * iv * param list)) * float SM.t
