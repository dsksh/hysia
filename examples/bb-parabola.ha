let g = 1
#let f = 0
let f = 0.1

var   x,y,vx,vy

#init  L, 1,10,0,0
#init  L, -1,1, 2/sqrt(2),1/sqrt(2)
init  L, -1,1, 1.4,0.7

at L wait vx,vy,0,-g+f*vy^2
  watch (x^2-y, vy-2*x*vx) goto L then 
    x,y,
    vx + (4*x*(vy - 2*x*vx)) / (4*x^2 + 1),
    vy - (2*(vy - 2*x*vx)) / (4*x^2 + 1)

#    vx - 1.9 * ((2*x*vx - vy) / (4*x^2 +1)) * 2*x,
#    vy + 1.9 * ((2*x*vx - vy) / (4*x^2 +1))
end

param order = 20
param t_max = 10
param h_max = 0.01
param dump_interval = 0.01
