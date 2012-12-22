external test: unit -> unit = "caml_test"
external test1: string -> unit = "caml_test1"

external init: int -> unit = "caml_init"
external put_variable: string -> int = "caml_put_variable"
external put_value: float -> float -> unit = "caml_put_value"

external put_var_node: int -> unit = "caml_put_var_node"
external put_scalar_node: float -> unit = "caml_put_scalar_node"

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

external put_tree: unit -> unit = "caml_put_tree"

external integrate: float -> float -> float -> float -> unit = "caml_integrate"
