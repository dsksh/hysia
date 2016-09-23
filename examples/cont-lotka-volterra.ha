let alpha = 0.666
let beta  = 1.333
let delta = 1
let gamma = 1

var   x, y

init  Loc, 1.8, 1.8
#init  Loc, 15+p1, 15+p2, 36+p3

at Loc wait alpha*x - beta*x*y, delta*x*y - gamma*y
end

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.02
#param dump_interval = 0.005
param char_mtx = -1
