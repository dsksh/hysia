type expr = expr_node Hashcons.hash_consed
and expr_node =
    Var of Model_common.ident
  | Val of Interval.t
  | App of Model_common.un_op * expr
  | App2 of Model_common.bin_op * expr * expr
module Expr_node :
  sig
    type t = expr_node
    val equal : 'a -> 'a -> bool
    val hash : expr_node -> int
  end
module Hexpr :
  sig
    type key = Expr_node.t
    val hashcons : key -> key Hashcons.hash_consed
    val iter : (key Hashcons.hash_consed -> unit) -> unit
    val stats : unit -> int * int * int * int * int * int
  end
module PMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
val mk_var : Model_common.ident -> Hexpr.key Hashcons.hash_consed
val mk_val : Interval.t -> Hexpr.key Hashcons.hash_consed
val mk_app : Model_common.un_op -> expr -> expr
val mk_app2 : Model_common.bin_op -> expr -> expr -> expr
val mk_expr : Interval.t PMap.t -> Ptree.expr -> expr
type dual = dual_node Hashcons.hash_consed
and dual_node = expr * expr list
module Dual_node :
  sig
    type t = dual_node
    val equal : 'a * 'b -> 'a * 'c -> bool
    val hash : 'a Hashcons.hash_consed * 'b -> int
  end
module Hdual :
  sig
    type key = Dual_node.t
    val hashcons : key -> key Hashcons.hash_consed
    val iter : (key Hashcons.hash_consed -> unit) -> unit
    val stats : unit -> int * int * int * int * int * int
  end
val diff_expr : Model_common.ident -> expr -> expr
val mk_dual :
  Model_common.ident list -> expr -> Hdual.key Hashcons.hash_consed
val mk_dual_expr :
  Interval.t PMap.t ->
  Model_common.ident list -> Ptree.expr -> Hdual.key Hashcons.hash_consed
val mk_normal :
  Model_common.ident list ->
  (expr * 'a) Hashcons.hash_consed list ->
  ('b * expr list) Hashcons.hash_consed -> Hdual.key Hashcons.hash_consed
val mk_edge :
  Interval.t PMap.t ->
  Model_common.ident list ->
  'a ->
  'b *
  ('c * Ptree.expr * ('d * Ptree.expr list) * ('e * 'f) *
   ('g * Ptree.expr list)) ->
  'c * Hdual.key Hashcons.hash_consed * Hdual.key Hashcons.hash_consed list *
  'f * Hdual.key Hashcons.hash_consed list
val mk_loc :
  Interval.t PMap.t ->
  Model_common.ident list ->
  ('a * ('b * expr list) Hashcons.hash_consed) list ->
  'c *
  (('d * 'e) * ('f * Ptree.expr list) * ('g * Ptree.expr list) *
   ('h *
    ('i *
     ('j * Ptree.expr * ('k * Ptree.expr list) * ('l * 'm) *
      ('n * Ptree.expr list)))
    list)) ->
  'e * Hdual.key Hashcons.hash_consed list *
  (Hdual.key Hashcons.hash_consed * Hdual.key Hashcons.hash_consed) list *
  ('j * Hdual.key Hashcons.hash_consed *
   Hdual.key Hashcons.hash_consed list * 'm *
   Hdual.key Hashcons.hash_consed list)
  list * ('b * expr list) Hashcons.hash_consed list *
  Hdual.key Hashcons.hash_consed list
val get_lid : 'a * Ptree.expr_node -> Model_common.ident
type mitl_formula =
    Mtrue
  | Mloc of int * Model_common.ident
  | Mexpr of dual
  | Mnot of mitl_formula
  | Mor of mitl_formula * mitl_formula
  | Muntil of Interval.t * mitl_formula * mitl_formula
val mk_mitl_formula :
  Interval.t PMap.t ->
  Model_common.ident list ->
  (int * Hdual.key Hashcons.hash_consed) list ->
  Model_common.ident list ->
  Ptree.mitl_node ->
  (int * Hdual.key Hashcons.hash_consed) list * Model_common.ident list *
  mitl_formula * float
val make :
  ('a * (PMap.key * Ptree.pval)) list * ('b * Model_common.ident) list *
  Ptree.expr list *
  ('c *
   (('d * 'e) * ('f * Ptree.expr list) * ('g * Ptree.expr list) *
    ('h *
     ('i *
      ('j * Ptree.expr * ('k * Ptree.expr list) * ('l * 'm) *
       ('n * Ptree.expr list)))
     list)))
  list ->
  'o * Ptree.mitl_node ->
  ((PMap.key * float) list * Model_common.ident list *
   (Model_common.ident * expr list) *
   ('e * Hdual.key Hashcons.hash_consed list *
    (Hdual.key Hashcons.hash_consed * Hdual.key Hashcons.hash_consed) list *
    ('j * Hdual.key Hashcons.hash_consed *
     Hdual.key Hashcons.hash_consed list * 'm *
     Hdual.key Hashcons.hash_consed list)
    list * Hdual.key Hashcons.hash_consed list *
    Hdual.key Hashcons.hash_consed list)
   list) *
  ((int * Hdual.key Hashcons.hash_consed) list * Model_common.ident list *
   mitl_formula * float)
val make_prop :
  Model_common.ident list ->
  'a * Ptree.mitl_node ->
  (int * Hdual.key Hashcons.hash_consed) list * Model_common.ident list *
  mitl_formula * float
type param = string * float
type id = Model_common.ident
type init = Model_common.ident * expr list
type dexpr = dual
type iexpr = dual * dual
type gexpr = dual
type rexpr = dual
type prop = mitl_formula
type edge = bool * gexpr * gexpr list * Model_common.ident * rexpr list
type location =
    Model_common.ident * dexpr list * iexpr list * edge list * dual list *
    dual list
val print_expr : Format.formatter -> expr -> unit
val print_dual :
  Format.formatter -> (expr * expr list) Hashcons.hash_consed -> unit
val print_param : Format.formatter -> string * float -> unit
val print_id : Format.formatter -> string -> unit
val print_init : Format.formatter -> string * expr list -> unit
val print_dexpr :
  Format.formatter -> (expr * expr list) Hashcons.hash_consed -> unit
val print_iexpr :
  Format.formatter ->
  (expr * expr list) Hashcons.hash_consed *
  (expr * expr list) Hashcons.hash_consed -> unit
val print_gexpr :
  Format.formatter -> (expr * expr list) Hashcons.hash_consed -> unit
val print_rexpr :
  Format.formatter -> (expr * expr list) Hashcons.hash_consed -> unit
val print_prop : Format.formatter -> mitl_formula -> unit
val id_of_loc : 'a * 'b * 'c * 'd * 'e * 'f -> 'a
val dexprs_of_loc : 'a * 'b * 'c * 'd * 'e * 'f -> 'b
val iexprs_of_loc : 'a * 'b * 'c * 'd * 'e * 'f -> 'c
val edges_of_loc : 'a * 'b * 'c * 'd * 'e * 'f -> 'd
val gf_of_edge : 'a * 'b * 'c * 'd * 'e -> 'a
val gh_of_edge : 'a * 'b * 'c * 'd * 'e -> 'b
val gg_of_edge : 'a * 'b * 'c * 'd * 'e -> 'c
val dst_of_edge : 'a * 'b * 'c * 'd * 'e -> 'd
val rexprs_of_edge : 'a * 'b * 'c * 'd * 'e -> 'e
