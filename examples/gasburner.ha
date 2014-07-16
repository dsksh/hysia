let H1 = 2
let H2 = 2
let A1 = 0.01
let A2 = 0.01
let B1 = 0.005
let B2 = 0.005

var   x1, x2

init  OnOff, 0, 50

at OnOff wait H1 - A1*x1 + B1*x2, -A2*x2 + B2*x1
  watch (x1-100, x2-80) goto OffOn  then x1, x2
  watch (x1-100, -x1)   goto OffOff then x1, x2
  watch (x2,     20-x1) goto OffOn  then x1, x2
end

at OffOff wait -A1*x1 + B1*x2, -A2*x2 + B2*x1
  watch (x1-80,  -x1)   goto OnOff  then x1, x2
  watch (x2-80,  -x2)   goto OffOn  then x1, x2
end

at OffOn wait -A1*x1 + B1*x2, H2 - A2*x2 + B2*x1
  watch (x2-100, -x2)   goto OffOff then x1, x2
  watch (x2-100, x2-80) goto OnOff  then x1, x2
  watch (x1,     x2-20) goto OnOff  then x1, x2
end


param order = 10
param t_max = 80
param h_min   = 1e-14
#param epsilon = 1e-14
param epsilon = 1e-8
#param delta = 1e-1
param dump_interval = 10
param char_mtx = 3
