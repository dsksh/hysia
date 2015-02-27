val invert_fs : ('a * bool) list option -> ('a * bool) list option
val cmp_fs : Interval.t * 'a -> Interval.t * 'b -> int
val intersect_fs :
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val shift_fs :
  float ->
  Interval.t ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val print_fs : Format.formatter -> (Interval.t * bool) list option -> unit
val check :
  bool ->
  float ->
  (int * (Interval.t * bool) list option) list ->
  Model.mitl_formula -> (Interval.t * bool) list option
val eval_at_zero : (Interval.t * bool) list option -> bool option
