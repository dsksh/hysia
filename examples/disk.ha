#let space = [-1e-4,1e-4]
let space = [-1e-5,1e-5]
#let space = [-1e-6,1e-6]

var   x1, x2

init  L, 1+space, space
#init  L, 1, 0

at L wait -x2, x1
  watch ((x1-1)^2 + x2^2 - 1, -2*x2) goto L then -x1+2, -x2
end

param order = 10
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param char_mtx = 1
