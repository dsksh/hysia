let g = 1
#let g = 10
let c = 1
#let c = 0.75
let f = 0.1
let y0 = 10

var   t, y, dy

init  L, 0, y0, 0

at L wait 1, dy, -g + f*dy^2
  watch (y, dy) goto L then t, y, -c*dy
end

param order = 20
param t_max = 20
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 1
#param delta = 0.99
#param tau = 1.01
param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 2
param qr_thres = 100
