open Format
open Model_common

let rec print_list sp fmtr fmt list =
  fprintf fmt "%a" (print_list_ sp "" fmtr) list

and print_list_ sp sp_ fmtr fmt = function
  | e::[] -> 
      fprintf fmt "%s%a" sp_ fmtr e
  | e::list -> 
      fprintf fmt "%s%a%a" sp_ fmtr e (print_list_ sp sp fmtr) list
  | [] -> ()


let print_float fmt v =
  fprintf fmt "%f" v

let print_interval fmt = function
  | Point v -> fprintf fmt "[%f]" v
  | Interval (l,u) -> fprintf fmt "[%f,%f]" l u

let print_rational fmt = function
  | _,(n,1) ->
      fprintf fmt "%d" n
  | _,(n,d) ->
      fprintf fmt "%d/%d" n d


(*let sprint_bin_op = function
  | Oadd -> "+"
  | Osub -> "-"
  | Omul -> "*"
  | Odiv -> "/" 
  | Opow -> "^" 

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

let rec print_expr fmt = function
  | _, Pvar id -> fprintf fmt "%s" id
  | _, Pint v  -> fprintf fmt "%d" v
  | _, Pval v  -> fprintf fmt "%f" v
  | _, Papp (op,e) -> 
      fprintf fmt "%s %a" (sprint_un_op op) print_expr e
  | _, Papp2 (op,e1,e2) -> 
      fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op) print_expr e2
*)

module type Printer = sig
  type var
  type der
  type init
  type grd
  type jump
  type param

  val print_var   : formatter -> var -> unit
  val print_der   : formatter -> der -> unit
  val print_init  : formatter -> init -> unit
  val print_grd   : formatter -> grd -> unit
  val print_jump  : formatter -> jump -> unit
  val print_param : formatter -> param -> unit
end

module Make (P : Printer) =
struct
  let print fmt (var,der,init,grd,jump,ps) =
    fprintf fmt "@[<hov 2>var:@ %a@];@ @[<hov 2>der:@ %a@];@ @[<hov 2>init:@ %a@];@ @[<hov 2>grd:@ %a@];@ @[<hov 2>jump:@ %a@];@ @[<hov 2>param:@ %a;@]"
      (print_list "," P.print_var) var
      (print_list "," P.print_der) der
      (print_list "," P.print_init) init
      P.print_grd grd
      (print_list "," P.print_jump) jump
      (print_list " " P.print_param) ps
end
