type polarity = Rise | Fall | Unknown
val print_polar : Format.formatter -> polarity -> unit
val invert_polar : polarity -> polarity
type signal =
    Strue
  | Sfalse
  | Sconst of float * float
  | Sexpr of string * int * polarity
  | Snot of signal
type flowpipe = (float * signal list) list
val print_signal : Format.formatter -> signal -> unit
val print_signals : Format.formatter -> signal list -> unit
val print_fp : Format.formatter -> (float * signal list) list -> unit
val negate_signal : signal -> signal
val check_prop_polar_ :
  string -> int -> 'a * 'b -> 'a * (float * signal list) list
val find_prop_extremum_ :
  float -> 'a * (float * signal list) list -> 'a * (float * signal list) list
val get_time_u : 'a -> ('a * 'b) list -> 'a
val minimize_signals_ :
  bool ->
  bool -> float -> float -> signal * signal list -> float * signal list
val merge_fps :
  float ->
  float ->
  (float * signal list) list ->
  (float * signal list) list -> (float * signal list) list
val param_values : (string * float) list Queue.t
val get_param_value : 'a * float -> 'a * float
val set_param_ : string -> string * float -> unit
val simulate :
  (string * float) list * 'a * (string * 'b) *
  (string * 'c * ('d * 'e) list * (bool * 'd * 'f * 'g * 'h) list * 'i * 'j)
  list -> ('k * 'l) list * 'm -> ('k * (float * signal list) list) list
val propagate :
  bool ->
  (int * (float * signal list) list) list ->
  Model.mitl_formula -> (float * signal list) list
val dump_signal : bool -> float -> float -> signal -> unit
val dump_signals : float -> (float * signal list) list -> unit
val dump_fp :
  'a * 'b * (string * 'c) * 'd -> (float * signal list) list -> unit
