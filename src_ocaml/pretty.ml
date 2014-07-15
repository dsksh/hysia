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


module type Printer = sig
  type param
  type id
  type init
  type dexpr
  type gexpr
  type rexpr
  type location
  type edge

  val print_param : formatter -> param -> unit
  val print_id    : formatter -> id -> unit
  val print_init  : formatter -> init -> unit
  val print_dexpr : formatter -> dexpr -> unit
  val print_gexpr : formatter -> gexpr -> unit
  val print_rexpr : formatter -> rexpr -> unit
  (*val print_edge  : formatter -> edge -> unit
  val print_location : formatter -> location -> unit*)
  val id_of_loc   : location -> id
  val dexprs_of_loc  : location -> dexpr list
  val edges_of_loc : location -> edge list
  val gh_of_edge  : edge -> gexpr
  val gg_of_edge  : edge -> gexpr list
  val dst_of_edge : edge -> id
  val rexprs_of_edge : edge -> rexpr list
end

module Make (P : Printer) =
struct
  let print_edge fmt edge =
    fprintf fmt "@;@[<hov 2>watch (%a, %a) @,goto %a @,then %a@]"
      P.print_gexpr (P.gh_of_edge edge) 
      (print_list "," P.print_gexpr) (P.gg_of_edge edge) 
      P.print_id (P.dst_of_edge edge)
      (print_list "," P.print_rexpr) (P.rexprs_of_edge edge)

  let print_location fmt loc =
    fprintf fmt "@;@[<hov 2>at %a wait (%a)@ [@[<hov 1>%a@]@;]@]" 
      P.print_id (P.id_of_loc loc)
      (print_list "," P.print_dexpr) (P.dexprs_of_loc loc)
      (print_list "" print_edge) (P.edges_of_loc loc)

  let print fmt (ps,vars,init,locs) =
    fprintf fmt "@[<hov 2>params:@ %a@];@;@[<hov 2>vars:@ %a@];@;@[<hov 2>init:@ %a@];@;@[<hov 2>locations:@ %a@]@]"
      (print_list " " P.print_param) ps
      (print_list "," P.print_id) vars
      P.print_init init
      (print_list "" print_location) locs
end
