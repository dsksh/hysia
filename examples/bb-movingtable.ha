let g = 1
#let c = 1
let c = 0.9
let f = 0
#let f = 0.04
let x0 = R 5
#let x0 = 1.4

var   t, x, vx

init  Fall, 0, 2+x0, 0

at Fall wait 1, vx, -g + f*vx^2
  once (sin(t)-x, vx-cos(t), -c*vx + (c+1)*cos(t)) goto Fall then t, x, -c*vx + (c+1)*cos(t)
  once (sin(t)-x, vx-cos(t),  c*vx - (c+1)*cos(t)) goto Rise then t, x, -c*vx + (c+1)*cos(t)
  (*once (x, vx) goto Rise then t, x, -vx*)
end

at Rise wait 1, vx, -g - f*vx^2
  once (vx, true) goto Fall then t, x, vx
  once (sin(t)-x, vx-cos(t)) goto Rise then t, x, -c*vx + (c+1)*cos(t)
end

#prop 2-x
#prop F (2-x)
#prop  G[2,10]!(x-5)
#prop  G[0,10](11-x)
#prop  F[5,10](x-2)
prop G[0,10](F[0,5](2-x))

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-12
param dump_interval = 0.01
