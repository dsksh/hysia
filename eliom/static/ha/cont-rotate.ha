#let   intv = [-1e-4,1e-4]
#let   intv = [-1e-8,1e-8]
let   intv = 0

let   c0 = 0.1
#let   c1 = 0
#let   c1 = [-1e-5,1e-5]
let   c1 = R 0.2
let   c2 = -1
let   c3 = 1
#let   c4 = -1
#let   c4 = 0
let   c4 = R 0.2

var   x1, x2

#init  Loc, 1+intv, intv
init  Loc, 1+intv, 0+intv

at Loc wait (c1-c0/2)*x1 + c2*x2, c3*x1 + (c4-c0/2)*x2
end

prop G[0,10](F[0,5](0.8-x1))

param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param dump_interval = 0.1
param dump_interval = 0.9
