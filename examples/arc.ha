let   intv = [-1e-5,1e-5]
#let   intv = [-1e-6,1e-6]

#let   c = 1
let   c = 2
let   o = 0
#let   o = 0.01

var   x1, x2, v

init  Loc, 1+intv, intv, 1

at Loc wait -v*x2, v*x1, 0
  once (c*x1-x2+o, -v*x1) goto Loc then x1, x2, -v
end

param order = 10
param t_max = 100
param dump_interval = 0.1
