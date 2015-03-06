type t = { inf : float; sup : float; }
external float_of_string : string -> int -> float = "caml_float_of_string_v"
val interval_of_float : float -> t
val interval_of_string : string -> string -> t
val zero : t
val one : t
val universe : t
val positive : t
val negative : t
val pi : t
external add_down : float -> float -> float = "caml_add_down"
external add_up : float -> float -> float = "caml_add_up"
external sub_down : float -> float -> float = "caml_sub_down"
external sub_up : float -> float -> float = "caml_sub_up"
external mul_down : float -> float -> float = "caml_mul_down"
external mul_up : float -> float -> float = "caml_mul_up"
external div_down : float -> float -> float = "caml_div_down"
external div_up : float -> float -> float = "caml_div_up"
external sqrt_down : float -> float -> float = "caml_sqrt_down"
external sqrt_up : float -> float -> float = "caml_sqrt_up"
val ( +$ ) : t -> t -> t
val ( +$. ) : t -> float -> t
val ( +.$ ) : float -> t -> t
val ( -$. ) : t -> float -> t
val ( -.$ ) : float -> t -> t
val ( -$ ) : t -> t
val ( *$ ) : t -> t -> t
val ( /$ ) : t -> t -> t
val print_interval : Format.formatter -> t -> unit
