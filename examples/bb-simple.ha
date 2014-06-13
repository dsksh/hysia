let g = 1
#let g = 10
#let c = 1
let c = 0.5
let f = 0.001

var   t, y, vy

#init  L, 0, 10, -0
init  L, 0, 10, -0.01

#at L wait 1, vy, -g
at L wait 1, vy, -g - f*sqrt(vy^2)*vy
  watch (y, vy) goto L then t, y, -c*vy
end

param order = 20
param t_max = 10
#param h_min   = 0.1
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.4
#param delta = 0.99
#param tau = 1.01
param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 2
param qr_thres = 8000
