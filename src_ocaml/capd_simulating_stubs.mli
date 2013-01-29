external initialize: unit -> unit = "sim_initialize"
external dispose: unit -> unit = "sim_dispose"
external find_first_zero: unit -> bool = "sim_find_first_zero"
external find_first_zero_mid: unit -> bool = "sim_find_first_zero_mid"
external simulate_jump: unit -> unit = "sim_simulate_jump"
external print_pped: bool -> bool -> unit = "sim_print_pped"

(*external integrate: float -> float -> float -> float -> unit = "sim_integrate"*)

