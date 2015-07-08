let g = 1
#let g = 10
#let c = 1
let c = 0.9
let f = 0.04
#let x0 = R 5
let x0 = 1.4

var   t, x, vx

#init  Fall, 0, 5+x0, 0
#init  Fall, 0, [6,6.00001], 0
#init  Fall, 0, 2+x0, 0
init  Fall, 0, 2+[0,0.002]+x0, 0
#init  Fall, 0, 3.86578580569671715+[0,0.01], 0
#init  Fall, 0, 2+[0,0.01]+1.65, 0
#init  Fall, 0, 4.17621061830895446, 0

#at Fall wait 1, vx, -g
at Fall wait 1, vx, -g + f*vx^2
  #watch F (sin(t)-x, vx-cos(t), -c*vx + (c+1)*cos(t)) goto Fall then t, x, -c*vx + (c+1)*cos(t)
  #watch F (sin(t)-x, vx-cos(t),  c*vx - (c+1)*cos(t)) goto Rise then t, x, -c*vx + (c+1)*cos(t)
  watch F (x, vx) goto Rise then t, x, -vx
end

at Rise wait 1, vx, -g - f*vx^2
  watch F (vx, true) goto Fall then t, x, vx
  watch F (sin(t)-x, vx-cos(t)) goto Rise then t, x, -c*vx + (c+1)*cos(t)
end

prop G[0,10](F[0,5](x-2))
#prop  G[2,10]!(x-5)
#prop  G[0,10](11-x)
#prop  F[5,10](x-2)

param order = 30
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-12
#param epsilon = 0
param dump_interval = 0.8
#param dump_interval = -1
#param delta = 0.99
#param tau = 1.01
param char_mtx = -1
#param char_mtx = 0
#param char_mtx = 3
param qr_thres = 8000
