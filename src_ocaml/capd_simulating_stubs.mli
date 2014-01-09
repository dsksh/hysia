external initialize: unit -> unit = "sim_initialize"
external dispose: unit -> unit = "sim_dispose"
(*external find_first_zero: string -> bool = "sim_find_first_zero"
external find_first_zero_mid: string -> bool = "sim_find_first_zero_mid"*)
external find_first_zero: bool -> string -> string -> float*float = "sim_find_first_zero"
external find_first_zero_mid: string -> string -> bool = "sim_find_first_zero_mid"
external simulate_jump: string -> string -> float -> float -> unit = "sim_simulate_jump"
external print_pped: bool -> bool -> unit = "sim_print_pped"
external report_step: int -> string -> unit = "sim_report_step"

(*external integrate: float -> float -> float -> float -> unit = "sim_integrate"*)

