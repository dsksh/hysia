#let intv = [-1e-4,1e-4]
#let intv = [-1e-8,1e-8]
let intv = 0

let p0 = 1
let p1 = R 2
let p2 = R 2
let p3 = R 2

let s = 10
let r = 28
#let b = 2.67
let b = 2.5

var   x1, x2, x3

init  Loc, 15, 15, 36
#init  Loc, 15+p1, 15+p2, 36+p3

#at Loc wait s*(x2-x1),  x1*(r-x3) - x2,  x1*x2 - b*x3
at Loc wait (s+p1-p0)*(x2-x1),  x1*((r+p2-p0)-x3) - x2,  x1*x2 - (b+p3-p0)*x3
end

#prop G[0,15] (!(-x1-15) -> F[0.5,5] G[0,1] !(-x1) )
#prop G[0,20] ( (x1-15) -> F[1,4] G[0,1] (-x1) )

prop G[0,15] (!(-x1-15) -> F[0.5,5] G[0,1] ((x1-10)^2 + (x2-10)^2 - 150) )

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.02
#param dump_interval = 0.005
param char_mtx = -1
