let H1 = 2
let H2 = 2
let A1 = 0.01
let A2 = 0.01
let B1 = 0.005
let B2 = 0.005

var   x1, x2

init  OnOff, 0, [50,50]

at OnOff wait H1 - A1*x1 + B1*x2, -A2*x2 + B2*x1
  once (x1-100, 80-x2)  goto OffOn  then x1, x2
  once (x1-100, x1-80, x2-80) goto OffOff then x1, x2
  once (x2,     x1-20)  goto OffOn  then x1, x2
end

at OffOff wait -A1*x1 + B1*x2, -A2*x2 + B2*x1
  once (x1-80,  true)   goto OnOff  then x1, x2
  once (x2-80,  true)   goto OffOn  then x1, x2
end

at OffOn wait -A1*x1 + B1*x2, H2 - A2*x2 + B2*x1
  once (x2-100, x1-80, x2-80) goto OffOff then x1, x2
  once (x2-100, 80-x1)  goto OnOff  then x1, x2
  once (x1,     20-x2)  goto OnOff  then x1, x2
end


(* MITL property to satisfy *)

prop  F[0,100]G[0,200](!(75-x1) /\ !(75-x2))


param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 10
