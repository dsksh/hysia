let g = 1
#let g = 10
let c = 0.75
let y0 = 10

var   t, y, dy

init  L, 0, y0, 0

at L wait 1, dy, -g
  watch (y, dy) goto L then t, y, -c*dy
end

param order = 10
param t_max = 10
param h_min   = 0.000000000000001
param epsilon = 0.000000000000001
#param epsilon = 0
param dump_interval = 0.1
#param delta = 0.99
#param tau = 1.01
