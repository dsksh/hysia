val print_fs : Format.formatter -> (Interval.t * bool) list option -> unit
val cmp_fs : Interval.t * 'a -> Interval.t * 'b -> int
val normalize_fs :
  (Interval.t * bool) list -> (Interval.t * bool) list option
val invert_fs :
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val join_fs :
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val intersect_fs :
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val shift_elem :
  Interval.t -> (Interval.t * bool) list -> (Interval.t * bool) list option
val map_pairs :
  (('a * bool) list -> 'b list option) -> ('a * bool) list -> 'b list option
val shift_fs :
  Interval.t ->
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val mod_intervals :
  bool ->
  'a ->
  (int * (Interval.t * bool) list option) list ->
  Model.mitl_formula -> (Interval.t * bool) list option
val eval_at_zero : (Interval.t * bool) list option -> bool option
