
let g = 1
#let g = 10
let c = 1
#let c = 0.9
let f = 0
#let f = 0.1

let pertb = 0
#let pertb = [-1e-1,1e-1]
#let pertb = [-1e-3,1e-3]
#let pertb = [-1e-11,1e-11]

var   t, y, vy

init  Loc, 0, 1+pertb, -0+pertb
#init  Loc, 0, 1+pertb, -0

at Loc wait 1, vy, -g
#at L wait 1, vy, -g - f*vy^2
  watch F (y, vy) goto Loc then t, y, -c*vy
end

param order = 10
param t_max = 100
#param h_min   = 0.1
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param dump_interval = -1
param dump_interval = 0.1
#param delta = 0.99
#param tau = 1.01
#param char_mtx = -1
param char_mtx = 2
#param char_mtx = 3
param qr_thres = 100
#param qr_thres = 15000
