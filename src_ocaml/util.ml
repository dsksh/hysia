open Format
open Ptree

type error = 
  (*| Unification of Type.t * Type.t*)
  | DimMismatch of int * int
  | SyntaxError

exception Error of error * loc
exception Warning of error * loc

let report fmt = function
  (*| Unification(t1,t2) ->
      fprintf fmt "%a and %a cannot be unified" Type.print t1 Type.print t2*)
  | DimMismatch (d1,d2) ->
      fprintf fmt "dimensions %d and %d mismatched" d1 d2
  | SyntaxError -> 
      fprintf fmt "syntax error"

let error e l = raise (Error(e,l))
let warning e l = raise (Warning(e,l))
