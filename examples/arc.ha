#let   intv = [-1e-3,1e-3]
#let   intv = [-2e-5,2e-5]
let   intv = [-1e-6,1e-6]

var   x1, x2, c

init  Loc, 1+intv, intv, 2
#init  Loc, 1+intv, 1+intv, 2

at Loc wait -c*x2, c*x1, 0
  #watch (x1-x2, -c*x2) goto Loc then x1, x2, -c
  #watch (x1-x2+1, -c*(x1+x2)) goto Loc then x1, x2, -c
  watch (2*x1-x2, -c*x1) goto Loc then x1, x2, -c
end

param order = 10
param t_max = 100
#param h_min   = 1e-0
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = -1
#param epsilon = 0

# char_mtx 1: QR, 2: inverse, 3: ident, -1: hybrid
param char_mtx = -1
#param char_mtx = 3
#param dump_interval = 0.1
