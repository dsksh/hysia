let g = 1
let f = 0
#let f = 0.1

var   x1,x2,y, vx1,vx2,vy

#init  L, 0.668693,0.8312,3, 2.34542,0.373903,2.80361
init  Loc, 1,1,4, 0,0,0
#init  L, -1,0,1, 2/sqrt(2),0.0001,1/sqrt(2)
#init  L, -1,0,1, 2/sqrt(2),0,1/sqrt(2)
#init  L, -1+[-1e-5,1e-5],[-1e-8,1e-8],1, 2/sqrt(2),0,1/sqrt(2)
#init  L, -1+[-1e-5,1e-5],[-1e-8,1e-8],1, 2/sqrt(2),0,1/sqrt(2)
#init  L, -1+[-1e-5,1e-5],0,1, 2/sqrt(2),0.0001,1/sqrt(2)
#init  L, -1,0,1, 1.4,0.0001,0.7

at Loc wait vx1,vx2,vy, 0,0,-g
  #watch (x1^2+x2^2-y, vy-2*x1*vx1-2*x2*vx2) goto Loc then 
  watch (x1^2/2+x2^2/2-y, vy-2*x1*vx1-2*x2*vx2) goto Loc then 
    x1,x2,y,
	vx1 + (4*x1*(vy - 2*vx1*x1 - 2*vx2*x2)) / (1 + 4*x1^2 + 4*x2^2),
    vx2 + (4*x2*(vy - 2*vx1*x1 - 2*vx2*x2)) / (1 + 4*x1^2 + 4*x2^2),
    vy  - (2*(vy - 2*vx1*x1 - 2*vx2*x2)) / (1 + 4*x1^2 + 4*x2^2)

#    vx1 + (4*x1*(vy - 2*x1*vx1)) / (4*x1^2 + 1),
#	vx2,
#    vy - (2*(vy - 2*x1*vx1)) / (4*x1^2 + 1)
end

param order = 20
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.01
#param dump_interval = -1
#param delta = 0.99
#param tau = 1.01

# char_mtx 1: QR, 2: inverse, 3: ident, -1: hybrid
param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 2

#param qr_thres = 100
