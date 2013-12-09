let g = 1
let f = 0.1

var   x1,x2,y, vx1,vx2,vy

#init  L, 0.668693,0.8312,3, 2.34542,0.373903,2.80361
init  L, 1,1,3, 1,0,0

at L wait vx1,vx2,vy, 0,0,-g+f*vy^2
  watch (x1^2+x2^2-y, vy-2*x1*vx1-2*x2*vx2) goto L then 
    x1,x2,y,
	vx1 + (4*x1*(vy - 2*vx1*x1 - 2*vx2*x2)) / (1 + 4*x1^2 + 4*x2^2),
    vx2 + (4*x2*(vy - 2*vx1*x1 - 2*vx2*x2)) / (1 + 4*x1^2 + 4*x2^2),
    vy  - (2*(vy - 2*vx1*x1 - 2*vx2*x2)) / (1 + 4*x1^2 + 4*x2^2)
end

param order = 10
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.02
#param delta = 0.99
#param tau = 1.01
param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 2
param qr_thres = 100
