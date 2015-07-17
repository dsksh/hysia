val print_bs : Format.formatter -> (Interval.t * bool) list option -> unit
val cmp_bs : Interval.t * 'a -> Interval.t * 'b -> int
val normalize_bs :
  (Interval.t * bool) list -> (Interval.t * bool) list option
val invert_bs :
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val join_bs :
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val intersect_bs :
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val shift_elem :
  Interval.t -> (Interval.t * bool) list -> (Interval.t * bool) list
val map_pairs : (('a * bool) list -> 'b list) -> ('a * bool) list -> 'b list
val shift_bs :
  Interval.t ->
  (Interval.t * bool) list option ->
  (Interval.t * bool) list option -> (Interval.t * bool) list option
val propagate :
  bool ->
  (int * (Interval.t * bool) list option) list ->
  Model.mitl_formula -> (Interval.t * bool) list option
val eval_at_zero : (Interval.t * bool) list option -> bool option
