type 'a elem = { value : 'a; mutable prev : 'a t; mutable next : 'a t; }
and 'a t = 'a elem option
val create : unit -> 'a option
val is_empty : 'a option -> bool
val insert_first : 'a elem option -> 'a -> 'a elem option
val insert_last : 'a elem option -> 'a -> 'a elem option
val first_elem : 'a option -> 'a
val last_elem : 'a elem option -> 'a elem
val first : 'a elem option -> 'a
val last : 'a elem option -> 'a
val remove_first : 'a elem option -> 'a t
val remove_last : 'a elem option -> 'a elem option
