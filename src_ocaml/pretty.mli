val print_list :
  string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val print_list_ :
  string ->
  string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val print_float : Format.formatter -> float -> unit
val print_rational : Format.formatter -> 'a * (int * int) -> unit
module type Printer =
  sig
    type param
    type id
    type init
    type dexpr
    type iexpr
    type gexpr
    type rexpr
    type location
    type edge
    type prop
    val print_param : Format.formatter -> param -> unit
    val print_id : Format.formatter -> id -> unit
    val print_init : Format.formatter -> init -> unit
    val print_dexpr : Format.formatter -> dexpr -> unit
    val print_iexpr : Format.formatter -> iexpr -> unit
    val print_gexpr : Format.formatter -> gexpr -> unit
    val print_rexpr : Format.formatter -> rexpr -> unit
    val print_prop : Format.formatter -> prop -> unit
    val id_of_loc : location -> id
    val dexprs_of_loc : location -> dexpr list
    val iexprs_of_loc : location -> iexpr list
    val edges_of_loc : location -> edge list
    val gh_of_edge : edge -> gexpr
    val gg_of_edge : edge -> gexpr list
    val dst_of_edge : edge -> id
    val rexprs_of_edge : edge -> rexpr list
  end
module Make :
  functor (P : Printer) ->
    sig
      val print_edge : Format.formatter -> P.edge -> unit
      val print_location : Format.formatter -> P.location -> unit
      val print_ha :
        Format.formatter ->
        P.param list * P.id list * P.init * P.location list -> unit
    end
