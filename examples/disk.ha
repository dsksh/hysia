#let space = [-1e-4,1e-4]
#let space = [-1e-5,1e-5]
let space = [-1e-6,1e-6]

var   x1, x2

init  Loc, 1+space, space
#init  Loc, 1, 0

at Loc wait -x2, x1
  watch ((x1-1)^2 + x2^2 - 1, -2*x2) goto Loc then -x1+2, -x2
end

param order = 10
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = -1

# char_mtx 1: QR, 2: inverse, 3: ident, -1: hybrid
param char_mtx = -1
#param char_mtx = 3
