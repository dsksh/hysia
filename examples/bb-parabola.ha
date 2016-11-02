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

init  Loc, 1,2+pertb,0,0

at Loc wait vx,vy,0,-g

  once (x^2-y, vy-2*x*vx) goto Loc then 
    x,y,
    vx + (4*x*(vy - 2*x*vx)) / (4*x^2 + 1),
    vy - (2*(vy - 2*x*vx)) / (4*x^2 + 1)
end

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.01
