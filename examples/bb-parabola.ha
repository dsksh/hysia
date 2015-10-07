#let pertb = [-0.002,0.002]
#let pertb = [-1e-4,1e-4]
#let pertb = [-1e-6,1e-6]
#let pertb = [-1e-8,1e-8]
#let pertb = [-1e-10,1e-10]
#let pertb = [-1e-14,1e-14]
let pertb = 0

let g = 1
let f = 0
#let f = 0.1

var   x,y,vx,vy

#init  Loc, 1+pertb,2+pertb,0,0
#init  Loc, 1+pertb,2+pertb,-0.00001,-0.00001

#init  Loc, 0+pertb,1+pertb,0,0
init  Loc, 1,2+pertb,0,0

#init  Loc, 1,10,0,-0.1
#init  Loc, -1+pertb,1+pertb, 2/sqrt(2),1/sqrt(2)

#init  Loc, -1/20,1/400, 1/(10*sqrt(2)), 1/sqrt(2)
#init  Loc, -1/4+pertb,1/16+pertb, 1/(2*sqrt(2)), 1/sqrt(2)
#init  Loc, -1/2+pertb,1/4+pertb, 1/sqrt(2), 1/sqrt(2)
#init  Loc, -2+pertb,4+pertb, 4/sqrt(2), 1/sqrt(2)
#init  Loc, -4+pertb,16+pertb, 8/sqrt(2), 1/sqrt(2)
#init  Loc, -10+pertb+pertb,100, 20/sqrt(2), 1/sqrt(2)

#init  Loc, -1+pertb,1/2+pertb, 2/sqrt(2),1/sqrt(2)

#init  Loc, -1+pertb,1+pertb, 1.4,0.7

at Loc wait vx,vy,0,-g
#at Loc wait vx,vy, -f*sqrt(vx^2+vy^2)*vx, -g-f*sqrt(vx^2+vy^2)*vy

  watch F (x^2-y, vy-2*x*vx) goto Loc then 
    x,y,
    vx + (4*x*(vy - 2*x*vx)) / (4*x^2 + 1),
    vy - (2*(vy - 2*x*vx)) / (4*x^2 + 1)
end

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.01
#param dump_interval = -1
#param delta = 0.99
#param tau = 1.01
param char_mtx = -1
#param char_mtx = 0
#param char_mtx = 3
#param qr_thres = 1000
