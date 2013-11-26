open Format
open Ptree

type error = 
  (*| Unification of Type.t * Type.t*)
  | DimMismatch of int * int
  | SyntaxError
  | FindZeroError
  | FindZeroMidError
  | SelectEarliestError of (float*float) * (float*float)

exception LError of error * loc
exception LWarning of error * loc
exception Error of error
exception Warning of error

let report fmt = function
  (*| Unification(t1,t2) ->
      fprintf fmt "%a and %a cannot be unified" Type.print t1 Type.print t2*)
  | DimMismatch (d1,d2) ->
      fprintf fmt "dimensions %d and %d mismatched" d1 d2
  | SyntaxError -> 
      fprintf fmt "syntax error"
  | FindZeroError -> 
      fprintf fmt "failed to find a zero crossing"
  | FindZeroMidError -> 
      fprintf fmt "failed to find the midpoint of the zero crossing"
  | SelectEarliestError _ -> 
      fprintf fmt "failed to find the earliest discrete change"

let error e l = raise (LError (e,l))
let warning e l = raise (LWarning (e,l))
let error e = raise (Error e)
let warning e = raise (Warning e)
