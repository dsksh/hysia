type polarity = Rise | Fall | Unknown
val print_polar : Format.formatter -> polarity -> unit
type flowpipe = (float * int * polarity) list
val check_prop_polar_ :
  string -> int -> 'a * 'b -> (float * (string * int * polarity) list) list
val invert_polar : polarity -> polarity
val find_prop_extremum_ :
  float ->
  int ->
  (float * (string * int * polarity) list) list ->
  (float * (string * int * polarity) list) list
val simulate :
  (string * float) list * 'a * (string * 'b) *
  (string * 'c * ('d * 'e) list * (bool * 'd * 'f * 'g * 'h) list * 'i * 'j)
  list ->
  ('k * 'l) list * 'm -> (float * (string * int * polarity) list) list list
