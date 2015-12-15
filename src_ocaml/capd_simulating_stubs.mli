external initialize: unit -> unit = "sim_initialize"
external dispose: unit -> unit = "sim_dispose"
external set_param: string -> string -> float -> unit = "sim_set_param"
external check_prop: string -> int -> int = "sim_check_prop"
external find_inv_frontier: string -> int -> float*float = "sim_find_inv_frontier"
external find_prop_frontier: string -> int -> bool -> float -> float -> float*float = "sim_find_prop_frontier"
external find_first_zero: bool -> string -> int -> float*float = "sim_find_first_zero"
external find_first_zero_mid: string -> int -> bool = "sim_find_first_zero_mid"
external check_prop_polar: string -> int -> int = "sim_check_prop_polar"
external find_prop_extremum: string -> int -> float -> float -> float*float = "sim_find_prop_extremum"
external compare_signals: string -> bool -> bool -> int -> int -> float -> float -> int*(float*float) = "sim_compare_signals"
external simulate_jump: string -> int -> float -> float -> unit = "sim_simulate_jump"
external simulate_cont: string -> float -> unit = "sim_simulate_cont"
external print_pped: bool -> bool -> unit = "sim_print_pped"
external report_step: int -> string -> unit = "sim_report_step"
external get_dump_data: unit -> string = "sim_get_dump_data"

(*external integrate: float -> float -> float -> float -> unit = "sim_integrate"*)

