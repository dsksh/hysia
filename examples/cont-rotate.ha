#let   intv = [-1e-2,1e-2]
#let   intv = [-1e-6,1e-6]
#let   intv = [-1e-8,1e-8]
let   intv = 0

let   c0 = 0.1
let   c1 = 0.1
#let   c1 = R 0.2
let   c2 = -1
let   c3 = 1

var   x1, x2

init  Loc, 1+intv, intv

at Loc wait (c1-c0+intv)*x1 + c2*x2, c3*x1 + (c1-c0+intv)*x2
end

#prop 0 (* 0>0 *)
#prop 1 (* 1>0 *)
#prop x2 (* x2>0 *)
#prop x2-0.75
#prop !(x2) (* not x2>0 <=> x2<=0 *)
#prop !(x2-0.1) (* x2 <= 0.1 *)
#prop !(x2+0.1) (* x2 >= -0.1 *)
#prop !G (0.75-x2)
prop F x2-0.75
#prop G (F x2-0.75)
#prop x1 | x2
#prop x2 & x1
#prop !x1-0.1 | !x2-0.1
#prop !x1-0.1 & !x2-0.1
#prop x1 & x1
#prop x1 & !x2
#prop !x1 & !x2
#prop true
#prop true & x1
#prop 1 & x1
#prop !false & x1

#prop G[0,100] F[0,6.284] ! 1-x2 (* x2>1 *)

#prop G[0,100] F[0,6.284]( ! 1-x2 & F[0,3.142] ! x2+1 )

#prop G[0,100] F[0,6.284](! 1-x2 & F[0,1.571](! x2 & F[0,1.571](! x2+1 & F[0,1.571] x2 )))

#prop G[0,100]
#  F[0,6.284]( !1-x2 & F[0,0.786](0.707-x2 & F[0,0.786]( !x2 & F[0,0.786]( !x2+0.707 & 
#  F[0,0.786]( !x2+1 & F[0,0.786](x2+0.707 & F[0,0.786](  x2 & F[0,0.786]  !0.707-x2 )))))))

# these are not verified successfully
#prop G[0,10] (F[0,3.1415]( x2 & F[0,3.1415] !x2 )) (* c1 = 0.1000001 *)
#prop G[0,20] F[0,6.284] 
#  ( 0.7071-x2 U[0,1.571] ( (!x2+0.7071) U[0,1.571] ( x2+0.7071 U[0,1.771] ! 0.7071-x2 )))
#prop G[0,20] F[0,6.284]
#  ( (! 0.7071-x2) U[0,0.786] ( 0.7071-x2 U[0,1.571] (! x2+0.7071) U[0,0.786] 
#  ( (! x2+1) & ( (! x2+0.7071) U[0,0.786] ( x2+0.7071 U[0,1.571] 
#  ( (! 0.7071-x2) U[0,0.786] ! 1-x2 ))))))

param order = 20
param h_min   = 1e-14
param epsilon = 1e-14
#param dump_interval = 0.01
param dump_interval = 0.2
