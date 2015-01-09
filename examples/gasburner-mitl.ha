let H1 = 2
let H2 = 2
let A1 = 0.01
let A2 = 0.01
let B1 = 0.005
let B2 = 0.005

var   x1, x2

init  OnOff0, 0, [50,50]

at OnOff0 wait H1 - A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant x1, 100-x1, x2, 100-x2

  watch (100-x1, x2-80) goto OffOn0  then x1, x2
  watch (100-x1, true)  goto OffOff0 then x1, x2
  watch (x2,     20-x1) goto OffOn0  then x1, x2
end

at OffOff0 wait -A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant x1-80, 100-x1, x2-80, 100-x2

  watch (x1-80,  true)   goto OnOff0  then x1, x2
  watch (x2-80,  true)   goto OffOn0  then x1, x2
end

at OffOn0 wait -A1*x1 + B1*x2, H2 - A2*x2 + B2*x1
  invariant x1, 100-x1, x2, 100-x2

  watch (100-x2, true)  goto OffOff0 then x1, x2
  watch (100-x2, x2-80) goto OnOff0  then x1, x2
  watch (x1,     20-x2) goto OnOff0  then x1, x2
end


(* MITL property to satisfy *)

prop  F[0,200]G[0,1000](!(75-x1) /\ !(75-x2))


(* solving parameters *)

param order = 20
param t_max = 100
param h_min   = 1e-14
#param epsilon = 1e-14
param epsilon = 1e-8
#param delta = 1e-1
param dump_interval = 1
param char_mtx = 3
