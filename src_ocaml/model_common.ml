
type rational = int * int

type ident = string

type un_op = Osqr | Osqrt | Oexp | Olog | Osin | Ocos | Oatan | Oasin | Oacos |
             Oneg

type bin_op = Oadd | Osub | Omul | Odiv | Opow |
              Oand | Oor

let sprint_un_op = function
  | Osqr -> "sqr"
  | Osqrt -> "sqrt"
  | Oexp -> "exp"
  | Olog -> "log"
  | Osin -> "sin"
  | Ocos -> "cos"
  | Oatan -> "atan"
  | Oasin -> "asin"
  | Oacos -> "acos"
  | Oneg -> "neg"

let sprint_bin_op = function
  | Oadd -> "+"
  | Osub -> "-"
  | Omul -> "*"
  | Odiv -> "/" 
  | Opow -> "^" 
  | Oand -> "/\\" 
  | Oor  -> "\\/" 


module MParam = Map.Make(String)
