let g = 10
let c = -1
let cr = 20

var   x,y,vx,vy

init  Loc, 0,15,1,0
#init  Loc, 1,15,1,1

at Loc wait vx, vy, -g*x/sqrt(x^2+y^2)^3, -g*y/sqrt(x^2+y^2)^3
  once (vy-5000, -vy)
    goto Loc then 

    x,y,vx,vy
end

#param order = 17
param order = 20
#param t_max = 10000
param t_max = 100
param h_min   = 0.1
#param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 100
