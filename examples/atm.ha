let pi = 3.141592
let cx = 0
let cy = 0
let rd = R 5

var   t, r, x, y, vx, vy 

init  Start, 0, 1+rd, 10, 10, -1, -1

at Start wait 0, 0, vx, vy, 0, 0
  watch F ((x-cx)^2+(y-cy)^2-r*sqrt(3), true) goto Enter then t,r,x,y,vx,vy
end

at Enter wait 0, 0, vx, vy, -vy, vx
  watch F ((x-cx)^2+(y-cy)^2-r, true) goto Cycle then 0,r,x,y,vx,vy
end

at Cycle wait 1, 0, vx, vy, vy, -vx
  watch F (pi*2/3-t, true) goto Leave then t,r,x,y,vx,vy
end

at Leave wait 0, 0, vx, vy, -vy, vx
  watch F ((x-cx)^2+(y-cy)^2-r*sqrt(3), true) goto Start then t,r,x,y,vx,vy
end

#prop  G[0,10](F[0,5](x-2))

param order = 20
param t_max = 100
#param h_min   = 0.1
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.1
#param delta = 0.99
#param tau = 1.01
#param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 2
param char_mtx = 3
param qr_thres = 8000
