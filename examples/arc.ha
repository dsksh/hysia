let   intv = [-1e-6,1e-6]

var   x1, x2, c

init  L, 1+intv, intv, 2

at L wait -c*x2, c*x1, 0
  watch (x1-x2, -c*x2) goto L then x1, x2, -c
end

param order = 10
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param char_mtx = 1
param dump_interval = 0.1
