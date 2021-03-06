open Format
open Ptree

type error = 
  (*| Unification of Type.t * Type.t*)
  | DimMismatch of int * int
  | SyntaxError
  | SyntaxUnsupported of string
  | UnknownId of string
  | CheckInvError of string * int
  | CheckPropError of int * int
  | FindZeroError
  | FindZeroMidError
  | SelectEarliestError of (float*float) * (float*float)
  | UnknownOverlap
  | Unsupported

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
  | SyntaxUnsupported msg ->
      fprintf fmt "unsupported syntax: %s" msg
  | UnknownId id ->
      fprintf fmt "id %s is unknown" id
  | CheckInvError (lid,iid) -> 
      fprintf fmt "failed to evaluate the invariant %d at location %s" iid lid
  | CheckPropError (id,apid) -> 
      (*fprintf fmt "failed to evaluate the atomic property %d(%d) at init time" id apid*)
      fprintf fmt "atomic property %d(%d) does not hold at init time" id apid
  | FindZeroError -> 
      fprintf fmt "failed to find a zero crossing"
  | FindZeroMidError -> 
      fprintf fmt "failed to find the midpoint of the zero crossing"
  | SelectEarliestError _ -> 
      fprintf fmt "failed to find the earliest discrete change"
  | UnknownOverlap -> 
      fprintf fmt "undecidable overlap of frontiers occurred"
  | Unsupported ->
      fprintf fmt "input contains an unsupported expression"

let error e l = raise (LError (e,l))
let warning e l = raise (LWarning (e,l))
let error e = raise (Error e)
let warning e = raise (Warning e)

let mapi f l =
  let wrap f (i,res) elem = (i+1,(f i elem)::res) in
  let _,l = List.fold_left (wrap f) (0,[]) l in
  List.rev l
