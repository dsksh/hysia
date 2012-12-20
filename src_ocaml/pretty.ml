open Format
open Ptree

let rec print_list sp fmtr fmt list =
  fprintf fmt "%a" (print_list_ sp "" fmtr) list

and print_list_ sp sp_ fmtr fmt = function
  | e::[] -> 
      fprintf fmt "%s%a" sp_ fmtr e
  | e::list -> 
      fprintf fmt "%s%a%a" sp_ fmtr e (print_list_ sp sp fmtr) list
  | [] -> ()


let sprint_bin_op = function
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


let print_var fmt (_,id) =
  fprintf fmt "%s" id

let print_float fmt v =
  fprintf fmt "%f" v

let print_rational fmt = function
  | _,(n,1) ->
      fprintf fmt "%d" n
  | _,(n,d) ->
      fprintf fmt "%d/%d" n d


let print_vf fmt (_,(var,def)) =
  fprintf fmt "var:@ %a;@ vf:@ (%a);" 
    (print_list "," print_var) var
    (print_list "," print_expr) def

let print_iv fmt (_,(t,v)) =
  fprintf fmt "iv%@%d:@ %a;" t 
    (print_list "," print_float) v

let print_param fmt (_,(id,v)) =
  fprintf fmt "%s:=%d@," id v


let print_ptree fmt (_,(vf,iv,ps)) =
  fprintf fmt "@[<hov 2>%a@]@ @[<hov 2>%a@]@ @[<hov 2>params:@ %a;@]"
    print_vf vf
    print_iv iv
    (print_list "" print_param) ps
