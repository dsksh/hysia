val invert_fs : ('a * bool) list option -> ('a * bool) list option
val cmp_fs : Model_common.interval * 'a -> Model_common.interval * 'b -> int
val intersect_fs :
  (Model_common.interval * bool) list option ->
  (Model_common.interval * bool) list option ->
  (Model_common.interval * bool) list option
val shift_fs :
  'a ->
  Model_common.interval ->
  (Model_common.interval * bool) list option ->
  (Model_common.interval * bool) list option
val print_fs :
  Format.formatter -> (Model_common.interval * bool) list option -> unit
val check :
  bool ->
  'a ->
  (int * (Model_common.interval * bool) list option) list ->
  Model.mitl_formula -> (Model_common.interval * bool) list option
val eval_at_zero : (Model_common.interval * bool) list option -> bool option
