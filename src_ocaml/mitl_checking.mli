val invert_fs :
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val cmp_fs : Interval.t * 'a -> Interval.t * 'b -> int
val merge_fs : (Interval.t * bool) list -> (Interval.t * bool) list option
val intersect_fs :
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val shift_fs :
  'a ->
  Interval.t ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val print_fs : Format.formatter -> (Interval.t * bool) list option -> unit
val mod_intervals :
  bool ->
  'a ->
  (int * (Interval.t * bool) list option) list ->
  Model.mitl_formula -> (Interval.t * bool) list option
val eval_at_zero : (Interval.t * bool) list option -> bool option
