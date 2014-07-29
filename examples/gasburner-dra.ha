let H1 = 2
let H2 = 2
let A1 = 0.01
let A2 = 0.01
let B1 = 0.005
let B2 = 0.005

let p1 = R 25
#let p1 = 0

var   x1, x2

init  OnOff0, 0, 50
final OnOff1, OffOff1, OffOn1

at OnOff0 wait H1 - A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant x1, 100-x1, x2, 100-x2

  watch (100-x1, x2-80) goto OffOn0  then x1, x2
  watch (100-x1, true)  goto OffOff0 then x1, x2
  watch (x2,     20-x1) goto OffOn0  then x1, x2

  watch F (75-x1, 75-x2) goto OnOff1  then x1, x2
  watch F (75-x2, 75-x1) goto OnOff1  then x1, x2
end

at OffOff0 wait -A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant x1-80, 100-x1, x2-80, 100-x2

  watch (x1-80,  true)   goto OnOff0  then x1, x2
  watch (x2-80,  true)   goto OffOn0  then x1, x2

  watch F (x1-75, 75-x2) goto OffOff1  then x1, x2
  watch F (x2-75, 75-x1) goto OffOff1  then x1, x2
end

at OffOn0 wait -A1*x1 + B1*x2, H2 - A2*x2 + B2*x1
  invariant x1, 100-x1, x2, 100-x2

  watch (100-x2, true)  goto OffOff0 then x1, x2
  watch (100-x2, x2-80) goto OnOff0  then x1, x2
  watch (x1,     20-x2) goto OnOff0  then x1, x2

  watch F (75-x1, 75-x2) goto OffOn1  then x1, x2
  watch F (75-x2, 75-x1) goto OffOn1  then x1, x2
end


at OnOff1 wait H1 - A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant x1, 100-x1, x2, 100-x2, x1-75, x2-75

  watch (100-x1, x2-80, 75-x1, 75-x2) goto OffOn1  then x1, x2
  watch (100-x1, 75-x1, 75-x2)        goto OffOff1 then x1, x2
  watch (x2,     x1-20, 75-x1, 75-x2) goto OffOn1  then x1, x2

  watch (x1-75, true) goto OnOff0 then x1, x2
  watch (x2-75, true) goto OnOff0 then x1, x2
end

at OffOff1 wait -A1*x1 + B1*x2, -A2*x2 + B2*x1
  invariant x1-80, 100-x1, x2-80, 100-x2, x1-75, x2-75

  watch (x1-80,  75-x1, 75-x2) goto OnOff1  then x1, x2
  watch (x2-80,  75-x1, 75-x2) goto OffOn1  then x1, x2

  watch (x1-75, true) goto OffOff0 then x1, x2
  watch (x2-75, true) goto OffOff0 then x1, x2
end

at OffOn1 wait -A1*x1 + B1*x2, H2 - A2*x2 + B2*x1
  invariant x1, 100-x1, x2, 100-x2, x1-75, 75-x2-75

  watch (100-x2, 75-x1, 75-x2)        goto OffOff1 then x1, x2
  watch (100-x2, x2-80, 75-x1, 75-x2) goto OnOff1  then x1, x2
  watch (x1,     20-x2, 75-x1, 75-x2) goto OnOff1  then x1, x2

  watch (x1-75, true) goto OffOn0 then x1, x2
  watch (x2-75, true) goto OffOn0 then x1, x2
end


param order = 10
param t_max = 80
param h_min   = 1e-14
#param epsilon = 1e-14
param epsilon = 1e-8
#param delta = 1e-1
param dump_interval = 10
param char_mtx = 3
