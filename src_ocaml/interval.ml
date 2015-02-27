
type t = { inf:float; sup:float }

type rdir = Down | Up | Nearest

external float_of_string: string -> rdir -> float = "caml_float_of_string_v"

let interval_of_float v = {inf=v;sup=v}

let interval_of_string v = {inf=float_of_string v Down; sup=float_of_string v Up}

let interval_of_string l u = {inf=float_of_string l Down; sup=float_of_string u Up}

let zero = {inf=0.; sup=0.}
let one  = {inf=1.; sup=1.}
let universe = {inf=neg_infinity; sup=infinity}
let positive = {inf=0.; sup=infinity}
let negative = {inf=neg_infinity; sup=0.}

let pi = interval_of_string "3.1415926535897932384626433832795028841971693993751"
                            "3.1415926535897932384626433832795028841971693993752"

external add_down: float -> float -> float = "caml_add_down"
external add_up:   float -> float -> float = "caml_add_up"
external sub_down: float -> float -> float = "caml_sub_down"
external sub_up:   float -> float -> float = "caml_sub_up"
external mul_down: float -> float -> float = "caml_mul_down"
external mul_up:   float -> float -> float = "caml_mul_up"
external div_down: float -> float -> float = "caml_div_down"
external div_up:   float -> float -> float = "caml_div_up"
external sqrt_down: float -> float -> float = "caml_sqrt_down"
external sqrt_up:   float -> float -> float = "caml_sqrt_up"

let (+$) x y = { inf = add_down x.inf y.inf; sup = add_up x.sup y.sup }

let (-$) x y = { inf = sub_down x.inf y.sup; sup = sub_up x.sup y.inf }

let ( *$ ) x y = 
    if x.inf > 0. then begin
        if y.inf > 0. then (* PP *)
            { inf = mul_down x.inf y.inf; sup = mul_up x.sup y.sup }
        else if y.sup < 0. then (* PN *)
            { inf = mul_down x.sup y.inf; sup = mul_up x.inf y.sup }
        else (* PM *)
            { inf = mul_down x.sup y.inf; sup = mul_up x.inf y.sup }
    end else if x.sup < 0. then begin
        if y.inf > 0. then (* NP *)
            { inf = mul_down x.inf y.sup; sup = mul_up x.sup y.inf }
        else if y.sup < 0. then (* NN *)
            { inf = mul_down x.sup y.sup; sup = mul_up x.inf y.inf }
        else (* NM *)
            { inf = mul_down x.inf y.sup; sup = mul_up x.inf y.inf }
    end else
        if y.inf > 0. then (* MP *)
            { inf = mul_down x.inf y.sup; sup = mul_up x.sup y.sup }
        else if y.sup < 0. then (* MN *)
            { inf = mul_down x.sup y.inf; sup = mul_up x.inf y.inf }
        else (* MM *)
            { inf = mul_down x.inf y.sup; sup = mul_up x.inf y.inf }


let (/$) x y = 
    if y.inf > 0. then begin
        if x.inf >= 0. then
            { inf = div_down x.inf y.sup; sup = div_up x.sup y.inf }
        else if x.sup <= 0. then
            { inf = div_down x.inf y.inf; sup = div_up x.sup y.sup }
        else
            { inf = div_down x.inf y.inf; sup = div_up x.sup y.inf }
    end else if y.sup < 0. then begin
        if x.inf >= 0. then
            { inf = div_down x.sup y.sup; sup = div_up x.inf y.inf }
        else if x.sup <= 0. then
            { inf = div_down x.sup y.inf; sup = div_up x.inf y.sup }
        else
            { inf = div_down x.sup y.sup; sup = div_up x.inf y.sup }
    end else
        raise Division_by_zero

