
(*module type S =
  sig
    type var
    type der
    type init
    type grd
    type jump
    type param

    type t = var list * der list * init list * grd * jump list * param list
  end
*)

type interval = 
  | Interval of float * float 
  | Point of float

type rational = int * int

type ident = string

type un_op = Osqr | Osqrt | Oexp | Olog | Osin | Ocos | Oatan | Oasin | Oacos

type bin_op = Oadd | Osub | Omul | Odiv | Opow

let sprint_un_op = function
  | Osqr -> "sqr"
  | Osqrt -> "sqrt"
  | Oexp -> "exp"
  | Olog -> "log"
  | Osin -> "sin"
  | Oatan -> "atan"
  | Oasin -> "asin"
  | Oacos -> "acos"

let sprint_bin_op = function
  | Oadd -> "+"
  | Osub -> "-"
  | Omul -> "*"
  | Odiv -> "/" 
  | Opow -> "^" 


module MParam = Map.Make(String)
