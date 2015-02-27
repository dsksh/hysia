open Interval

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
  | Interval of Interval.t
  | Empty

type rational = int * int

type ident = string

type un_op = Osqr | Osqrt | Oexp | Olog | Osin | Ocos | Oatan | Oasin | Oacos |
             Oneg

type bin_op = Oadd | Osub | Omul | Odiv | Opow |
              Oand | Oor

let print_interval fmt = function
  | Interval v -> Format.fprintf fmt "[%f;%f]" v.inf v.sup
  (*| Point v -> Format.fprintf fmt "[%f]" v
  | Universe -> Format.fprintf fmt "(-oo,oo)"*)
  | Empty -> Format.fprintf fmt "(empty)"

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
