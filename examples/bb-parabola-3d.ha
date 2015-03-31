#let pertb = [-1e-10,1e-10]
let pertb = [-1e-12,1e-12]
#let pertb = 0

let g = 1
let c = 1

var   y,x1,x2,
      vy,vx1,vx2

init  Loc,
      #2+pertb,1+pertb,1+pertb,
      #1+pertb,0+pertb,0+pertb,
      1+pertb,0,0,
      0,0,0

at Loc wait vy,vx1,vx2,
            -g,0,0
  watch (x1^2/2+x2^2/2-y,
         vy-2*x1*vx1/2-2*x2*vx2/2) goto Loc then 
    y,x1,x2,
    vy  - (2*2*(2*vy - 2*x1*vx1 - 2*x2*vx2)) / (2^2 + 4*x1^2 + 4*x2^2),
    vx1 + (4*x1*(2*vy - 2*x1*vx1 - 2*x2*vx2)) / (2^2 + 4*x1^2 + 4*x2^2),
    vx2 + (4*x2*(2*vy - 2*x1*vx1 - 2*x2*vx2)) / (2^2 + 4*x1^2 + 4*x2^2)
end

param order = 20
param t_max = 10
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.1
#param dump_interval = -1
#param delta = 0.99
#param tau = 1.01

# char_mtx 1: QR, 2: inverse, 3: ident, -1: hybrid
param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 3
#param qr_thres = 100

