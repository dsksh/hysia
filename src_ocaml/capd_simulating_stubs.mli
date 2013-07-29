external initialize: unit -> unit = "sim_initialize"
external dispose: unit -> unit = "sim_dispose"
external find_first_zero: string -> bool = "sim_find_first_zero"
external find_first_zero_mid: string -> bool = "sim_find_first_zero_mid"
external simulate_jump: string -> unit = "sim_simulate_jump"
external print_pped: bool -> bool -> unit = "sim_print_pped"

(*external integrate: float -> float -> float -> float -> unit = "sim_integrate"*)

