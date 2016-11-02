let alpha = 0.666
let beta  = 1.333
let delta = 1
let gamma = 1

var   x, y

init  Loc, 1.8, 1.8

at Loc wait alpha*x - beta*x*y, delta*x*y - gamma*y
end

param order = 20
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.02
