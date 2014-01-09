#let   intv = [-1e-4,1e-4]
#let   intv = [-1e-5,1e-5]
let   intv = [-1e-8,1e-8]

var   x1, x2, c

#init  L, 1+intv, intv, 2
init  L, 1+intv, 1+intv, 2

at L wait -c*x2, c*x1, 0
  #watch (x1-x2, -c*x2) goto L then x1, x2, -c
  watch (x1-x2+1, -c*(x1+x2)) goto L then x1, x2, -c
end

param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param char_mtx = -1
#param char_mtx = 3
param dump_interval = 0.1
