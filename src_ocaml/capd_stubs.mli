external test: unit -> unit = "caml_test"
external test1: string -> unit = "caml_test1"

external initialize: int -> unit = "caml_init"
external put_variable: string -> int = "caml_put_variable"
external set_param: string -> float -> float -> int = "caml_set_param"

external put_var_node: int -> unit = "caml_put_var_node"
external put_scalar_node: float -> float -> unit = "caml_put_scalar_node"

external put_sqr_node:  unit -> unit = "caml_put_sqr_node"
external put_sqrt_node: unit -> unit = "caml_put_sqrt_node"
external put_exp_node:  unit -> unit = "caml_put_exp_node"
external put_log_node:  unit -> unit = "caml_put_log_node"
external put_sin_node:  unit -> unit = "caml_put_sin_node"
external put_cos_node:  unit -> unit = "caml_put_cos_node"
external put_atan_node: unit -> unit = "caml_put_atan_node"
external put_asin_node: unit -> unit = "caml_put_asin_node"
external put_acos_node: unit -> unit = "caml_put_acos_node"

external put_sum_node: unit -> unit = "caml_put_sum_node"
external put_dif_node: unit -> unit = "caml_put_dif_node"
external put_mul_node: unit -> unit = "caml_put_mul_node"
external put_div_node: unit -> unit = "caml_put_div_node"
external put_pow_node: unit -> unit = "caml_put_pow_node"

external put_der_tree:  string -> int -> unit = "caml_put_der_tree"
external put_der_dtree: string -> int -> int -> unit = "caml_put_der_dtree"
external done_der_tree: string -> unit -> unit = "caml_done_der_tree"

(*external put_value: float -> float -> unit = "caml_put_value"*)
external put_value: unit -> unit = "caml_put_value"

external put_grd_tree:  string -> string -> int -> unit = "caml_put_grd_tree"
external put_grd_dtree: string -> string -> int -> int -> unit = "caml_put_grd_dtree"

external put_jump_tree:  string -> string -> int -> unit = "caml_put_jump_tree"
external put_jump_dtree: string -> string -> int -> int -> unit = "caml_put_jump_dtree"

external put_edge: string -> string -> unit = "caml_put_edge"
external put_location: string -> unit = "caml_put_location"


external set_solving_param: string -> float -> unit = "caml_set_solving_param"
