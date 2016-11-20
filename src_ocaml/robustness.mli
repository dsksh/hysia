type polarity = Rise | Fall | Unknown
val print_polar : Format.formatter -> polarity -> unit
val invert_polar : polarity -> polarity
type signal =
    Strue
  | Sfalse
  | Sconst of Interval.t
  | Sexpr of string * int * polarity
  | Snot of signal
  | Sshift of float * signal
type flowpipe = (float * signal list) list
val print_signal : Format.formatter -> signal -> unit
val print_signals : Format.formatter -> signal list -> unit
val print_fp : Format.formatter -> (float * signal list) list -> unit
val proc_not : signal -> signal
val check_prop_polar_ :
  string -> int -> 'a * 'b -> 'a * (float * signal list) list
val find_prop_extremum_ :
  float -> 'a * (float * signal list) list -> 'a * (float * signal list) list
val get_time_u : 'a -> ('a * 'b) list -> 'a
val minimize_signals_ :
  bool ->
  bool ->
  bool ->
  float ->
  float -> float -> float -> signal * signal list -> float * signal list
val proc_and :
  float ->
  float ->
  (float * signal list) list ->
  (float * signal list) list -> (float * signal list) list
val proc_or :
  float ->
  float ->
  (float * signal list) list ->
  (float * signal list) list -> (float * signal list) list
val value_at_ : float -> bool -> signal -> Interval.t
val compare_vs : Interval.t -> Interval.t -> int
val find_intersection_ :
  bool -> bool -> signal -> signal -> float -> float -> float * float
val proc_evt_ut_ :
  bool ->
  bool ->
  float ->
  float ->
  (float * signal list) list ->
  signal -> signal list -> (float * signal list) list * signal
val proc_evt_ut :
  bool ->
  float ->
  float * signal list ->
  (float * signal list) list * signal -> (float * signal list) list * signal
val nth_time : ('a * 'b) list -> int -> 'a
val nth_sig : ('a * 'b list) list -> int -> 'b
val proc_evt_ :
  float ->
  float ->
  float ref ->
  float ->
  float ref ->
  int Dlist.t ref ->
  int ref ->
  (float * signal list) list ->
  (float * signal list) list -> (float * signal list) list
val shift_sig : float -> signal -> signal
val shift_pipe :
  float ->
  float ->
  float * signal list ->
  (float * signal list) list -> (float * signal list) list
val proc_evt :
  float ->
  float ->
  float -> float -> (float * signal list) list -> (float * signal list) list
val propagate :
  bool ->
  (int * (float * signal list) list) list ->
  Model.mitl_formula -> (float * signal list) list
val param_values : (string * float) list Queue.t
val get_param_value : 'a * float -> 'a * float
val set_param_ : string -> string * float -> unit
val simulate :
  (string * float) list * 'a * (string * 'b) *
  (string * 'c * ('d * 'e) list * (bool * 'd * 'f * 'g * 'h) list * 'i * 'j)
  list -> ('k * 'l) list * 'm -> ('k * (float * signal list) list) list
val dump_signal : bool -> float -> float -> float -> signal -> unit
val dump_signals : float -> (float * signal list) list -> unit
val dump_fp :
  'a * 'b * (string * 'c) * 'd -> (float * signal list) list -> unit
