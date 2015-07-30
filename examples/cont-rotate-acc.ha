#let   intv = [-1e-2,1e-2]
#let   intv = [-1e-4,1e-4]
#let   intv = [-1e-8,1e-8]
let   intv = 0

let   c1 = 0
let   c2 = -1
let   c3 = 1
let   c4 = 0

var   x1, x2, x3

#init  Loc, 1+intv, intv, 0
init  Loc, 1, 0, 0

#at Loc wait (c1+intv)*x1*x3^2 + c2*x2*x3^2, c3*x1*x3^2 + (c4+intv)*x2*x3^2, 1
at Loc wait c1*x1*x3^2 + (c2+intv)*x2*x3^2, (c3+intv)*x1*x3^2 + c4*x2*x3^2, 1
end

prop G[0,2] F[0,5]((x1+0.001) & G[0,0.5]!(x1-0.001))

#prop G[0,15] F[0,1.5](x2) (* 1<=x2 *)
#prop G[0,1] F[0,6.284]!(1-x2) (* 1<=x2 *)
#prop G[0,100] F[0,6.284](!(1-x2) & F[0,3.1416]!(x2+1))
#prop G[0,100] F[0,6.284](!(1-x2) & F[0,1.571]((!x2) & F[0,1.571](!(x2+1) & F[0,1.571](x2) )))
#prop G[0,20]
#  F[0,6.284](!(1-x2) & F[0,0.786]((-x2+0.7071) & F[0,0.786](!(x2) & F[0,0.786](!(x2+0.7071) & 
#  F[0,0.786](!(x2+1) & F[0,0.786]( (x2+0.7071) & F[0,0.786]( (x2) & F[0,0.786]  (x2+0.7071) )))))))

param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.1
#param char_mtx = -1
param char_mtx = 1
