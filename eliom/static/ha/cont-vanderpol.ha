#let   intv = [-1e-4,1e-4]
#let   intv = [-1e-8,1e-8]
let   intv = 0

let   mu = 5

var   x1, x2

init  Loc, 2, 0

at Loc wait x2, mu*(1-x1^2)*x2 - x1
end

#prop G[0,10](F[0,5](x1-0.8))

param order = 11
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1
