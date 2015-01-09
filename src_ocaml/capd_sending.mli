module SM :
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
val send_var : int SM.t -> SM.key -> int SM.t
val send_param : int SM.t -> SM.key * 'a -> int SM.t
val fun_un_op : Model_common.un_op -> unit -> unit
val fun_bin_op : Model_common.bin_op -> unit -> unit
val send_expr : int SM.t -> Model.expr -> unit
val send_dual :
  ('a -> 'b) ->
  ('a -> int -> 'c) ->
  int SM.t ->
  'a -> (Model.expr * Model.expr list) Hashcons.hash_consed -> 'c list
val send_der :
  string ->
  int SM.t ->
  int -> (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_inv :
  string ->
  int SM.t ->
  int ->
  (Model.expr * Model.expr list) Hashcons.hash_consed *
  (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_ap :
  string ->
  int SM.t ->
  int -> (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_ap_norm :
  string ->
  int SM.t ->
  int -> (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_grd :
  string ->
  int ->
  int ->
  int SM.t ->
  (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_grd_h :
  string ->
  int ->
  int SM.t ->
  (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_grd_g :
  string ->
  int ->
  int SM.t ->
  int -> (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_jump :
  string ->
  int ->
  int SM.t ->
  int -> (Model.expr * Model.expr list) Hashcons.hash_consed -> unit list
val send_init : int SM.t -> Model.expr list -> unit list
val send_edge :
  string ->
  int SM.t ->
  int ->
  'a * (Model.expr * Model.expr list) Hashcons.hash_consed *
  (Model.expr * Model.expr list) Hashcons.hash_consed list * string *
  (Model.expr * Model.expr list) Hashcons.hash_consed list -> unit list list
val send_loc :
  int SM.t ->
  string * (Model.expr * Model.expr list) Hashcons.hash_consed list *
  ((Model.expr * Model.expr list) Hashcons.hash_consed *
   (Model.expr * Model.expr list) Hashcons.hash_consed)
  list *
  ('a * (Model.expr * Model.expr list) Hashcons.hash_consed *
   (Model.expr * Model.expr list) Hashcons.hash_consed list * string *
   (Model.expr * Model.expr list) Hashcons.hash_consed list)
  list * (Model.expr * Model.expr list) Hashcons.hash_consed list *
  (Model.expr * Model.expr list) Hashcons.hash_consed list -> unit
val send_model :
  (SM.key * float) list * SM.key list * ('a * Model.expr list) *
  (string * (Model.expr * Model.expr list) Hashcons.hash_consed list *
   ((Model.expr * Model.expr list) Hashcons.hash_consed *
    (Model.expr * Model.expr list) Hashcons.hash_consed)
   list *
   ('b * (Model.expr * Model.expr list) Hashcons.hash_consed *
    (Model.expr * Model.expr list) Hashcons.hash_consed list * string *
    (Model.expr * Model.expr list) Hashcons.hash_consed list)
   list * (Model.expr * Model.expr list) Hashcons.hash_consed list *
   (Model.expr * Model.expr list) Hashcons.hash_consed list)
  list -> 'c -> unit
val send_solving_params : float Model_common.MParam.t -> unit
