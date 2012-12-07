
type loc = Lexing.position * Lexing.position
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let dummy_vf = dummy_loc, ([],[])
let dummy_iv = dummy_loc, (0,[])

type bin_ops =
  | Oadd | Osub | Omul | Odiv | Opow

type un_ops =
  | Osqr | Osqrt | Oexp | Olog | Osin | Ocos | Oatan | Oasin | Oacos

type var = loc * string

type rational = loc * (int * int)

type expr = loc * expr_node
and  expr_node =
  | Pvar of string
  | Pint of int
  | Papp of un_ops * expr
  | Papp2 of bin_ops * expr * expr

type vf = loc * (var list * expr list)

type iv = loc * (int * rational list)

type param = loc * (string * rational)

type model = loc * (vf * iv * param list)
