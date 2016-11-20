val step_max : int ref
val time_max : float ref
val loc_of_name : 'a -> 'a * 'b * 'c * 'd * 'e * 'f -> bool
val find_inv_frontier_ : string -> int -> 'a -> int * (float * float)
val select_earliest :
  ('a * (float * float)) option ->
  'a * (float * float) -> ('a * (float * float)) option
val find_first_zero_ :
  string ->
  bool * 'a * 'b * 'c * 'd ->
  int * (int * (float * float)) list * (int * (float * float)) list ->
  int * (int * (float * float)) list * (int * (float * float)) list
val select_earliest_grd :
  'a ->
  ('b * 'c) list ->
  ('d * 'b * 'e * 'f * 'g) list ->
  (int * (float * float)) option * (int * (float * float)) option ->
  int * (float * float) ->
  (int * (float * float)) option * (int * (float * float)) option
val filter_invariant :
  'a ->
  ('b * 'c) list ->
  ('d * 'b * 'e * 'f * 'g) list ->
  (int * (float * float)) option * (int * (float * float)) option ->
  int * (float * float) -> bool
val select_random : ('a * ('b * 'b)) list -> ('a * ('b * 'b)) option
val dst_of_edge : 'a * 'b * 'c * 'd * 'e -> 'd
val set_param_ : string -> string * float -> unit
val check_prop_ : string -> int -> int * 'a -> bool ref
val find_prop_frontier_ :
  string ->
  float ->
  float ->
  bool ref ->
  int -> 'a * (Interval.t * bool) list -> 'a * (Interval.t * bool) list
val simulate :
  (string * float) list * 'a * (string * 'b) *
  (string * 'c * ('d * 'e) list * (bool * 'd * 'f * string * 'g) list * 'h *
   'i)
  list ->
  (int * 'j) list * string list -> (int * (Interval.t * bool) list) list
