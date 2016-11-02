#let space = [-1e-4,1e-4]
let space = [-1e-6,1e-6]
#let space = 0

var   x1, x2

init  Loc, 1+space, space

at Loc wait -x2, x1
  once ((x1-1)^2 + x2^2 - 1, -2*x2) goto Loc then -x1+2, -x2
end

param order = 10
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1
