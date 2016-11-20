let H1 = 2
let H2 = 2
let A1 = 0.01
let A2 = 0.01
let B1 = 0.005
let B2 = 0.005

var   x1, x2

init  OnOff, 0, [50,50]

at OnOff wait H1 - A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant 100-x1, x1, 100-x2, x2

  watch (100-x1, 80-x2) goto OffOn  then x1, x2
  watch (100-x1, true)  goto OffOff then x1, x2
  watch (x2,     x1-20) goto OffOn  then x1, x2
end

at OffOff wait -A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant 100-x1, x1-80, 100-x2, x2-80

  watch (x1-80,  true)  goto OnOff  then x1, x2
  watch (x2-80,  true)  goto OffOn  then x1, x2
end

at OffOn wait -A1*x1 + B1*x2, H2 - A2*x2 + B2*x1
  invariant 100-x1, x1, 100-x2, x2

  watch (x2-100, true)  goto OffOff then x1, x2
  watch (x2-100, 80-x2) goto OnOff  then x1, x2
  watch (x1,     x2-20) goto OnOff  then x1, x2
end


(* solving parameters *)

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 1
