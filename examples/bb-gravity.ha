let g = 10
let c = -1
let cr = 20

var   x,y,vx,vy

init  L, 0,15,1,0

at L wait vx, vy, -g*x/sqrt(x^2+y^2)^3, -g*y/sqrt(x^2+y^2)^3
  watch (vy-5000, -vy)
    goto L then 

    x,y,vx,vy
end

param order = 20
param t_max = 10000
param h_min   = 0.1
#param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param char_mtx = -1
param char_mtx = 1
#param char_mtx = 2
param qr_thres = 100
param dump_interval = 100
