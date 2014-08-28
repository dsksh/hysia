let g = 1
#let g = 10
#let c = 1
let c = 0.8
let f = 0.001

var   x, y, vx, vy

init  L, 2, 3, 0, 0

at L wait vx, vy, 0, -g
#at L wait vx, vy, 0, -g - f*sqrt(vy^2)*vy

  watch F (sin(x) - y, vy - cos(x)*vx) goto L then 
    x, y,
    (vx + cos(x)*(vy + c*vy - c*vx*cos(x))) / (1 + cos(x)^2),
    (-c*vy + cos(x)*(vx + c*vx + vy*cos(x))) / (1 + cos(x)^2)
end

param order = 100
param t_max = 50
#param h_min   = 0.1
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.4
#param delta = 0.99
#param tau = 1.01
#param char_mtx = -1
param char_mtx = 1
#param char_mtx = 2
param qr_thres = 100
