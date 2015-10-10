#let   intv = [-1e-3,1e-3]
#let   intv = [-1e-4,1e-4]
let   intv = [-1e-5,1e-5]
#let   intv = [-1e-6,1e-6]
#let   intv = [-1e-8,1e-8]

#let   c = 1
let   c = 2
#let   o = 0
let   o = 0.01

var   x1, x2

#init  Loc1, 1+intv, intv
init  Loc1, 1+intv, 0+intv

at Loc1 wait -x2, x1
  watch (c*x1-x2+o, -x1) goto Loc2 then x1, x2
end

at Loc2 wait -x2, x1
  watch (c*x1-x2+o, x1) goto Loc1 then x1, x2
end

param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.1
#param dump_interval = -1
