type loc = Lexing.position * Lexing.position
type pval = PVint of Interval.t | PVrandom of float
type param = loc * (string * pval)
type id = loc * Model_common.ident
type lid = loc * Model_common.ident
type expr = loc * expr_node
and expr_node =
    Pvar of Model_common.ident
  | Pint of int
  | Pval of Interval.t
  | Papp of Model_common.un_op * expr
  | Papp2 of Model_common.bin_op * expr * expr
type mitl_formula = loc * mitl_node
and mitl_node =
    Ptrue
  | Ploc of Model_common.ident
  | Pexpr of expr
  | Pnot of mitl_node
  | Pand of mitl_node * mitl_node
  | Puntil of Interval.t * mitl_node * mitl_node
type id_l = loc * id list
type expr_l = loc * expr list
type interval_l = loc * Interval.t list
type edge = loc * (bool * expr * expr_l * lid * expr_l)
type edge_l = loc * edge list
type location = loc * (lid * expr_l * expr_l * edge_l)
type t = (param list * id_l * expr_l * location list) * mitl_formula
type init = expr list
type dexpr = expr
type iexpr = expr
type gexpr = expr
type rexpr = expr
type prop = mitl_formula
val simplify : 'a * ('b * 'c) * ('d * 'e) * 'f -> 'a * 'c * 'e * 'f
val dummy_loc : Lexing.position * Lexing.position
val dummy_list : (Lexing.position * Lexing.position) * 'a list
val dummy_grd : (Lexing.position * Lexing.position) * expr_node
val dummy_prop : (Lexing.position * Lexing.position) * mitl_node
val print_expr : Format.formatter -> expr -> unit
val print_prop : Format.formatter -> 'a * mitl_node -> unit
val print_prop_node : Format.formatter -> mitl_node -> unit
val print_param : Format.formatter -> 'a * (string * pval) -> unit
val print_id : Format.formatter -> 'a * string -> unit
val print_init : Format.formatter -> expr list -> unit
val print_dexpr : Format.formatter -> expr -> unit
val print_iexpr : Format.formatter -> expr -> unit
val print_gexpr : Format.formatter -> expr -> unit
val print_rexpr : Format.formatter -> expr -> unit
val id_of_loc : 'a * ('b * 'c * 'd * 'e) -> 'b
val dexprs_of_loc : 'a * ('b * ('c * 'd) * 'e * 'f) -> 'd
val iexprs_of_loc : 'a * ('b * 'c * ('d * 'e) * 'f) -> 'e
val edges_of_loc : 'a * ('b * 'c * 'd * ('e * 'f)) -> 'f
val gh_of_edge : 'a * ('b * 'c * 'd * 'e * 'f) -> 'c
val gg_of_edge : 'a * ('b * 'c * ('d * 'e) * 'f * 'g) -> 'e
val dst_of_edge : 'a * ('b * 'c * 'd * 'e * 'f) -> 'e
val rexprs_of_edge : 'a * ('b * 'c * 'd * 'e * ('f * 'g)) -> 'g
