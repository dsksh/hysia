type error =
    DimMismatch of int * int
  | SyntaxError
  | UnknownId of string
  | FindZeroError
  | FindZeroMidError
  | SelectEarliestError of (float * float) * (float * float)
  | UnknownOverlap

exception LError of error * Ptree.loc
exception LWarning of error * Ptree.loc
exception Error of error
exception Warning of error

val report : Format.formatter -> error -> unit
val error : error -> 'a
val warning : error -> 'a
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
