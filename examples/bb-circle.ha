let g = 10
let c = -1
let cx = 0
let cy = 0
let cr = 20

var   x,y,vx,vy

init  L, 0,30,0.2,0

at L wait vx, vy, -g*x/sqrt(x^2+y^2)^3, -g*y/sqrt(x^2+y^2)^3
  watch (x^2+y^2-cr^2, 
#      (y*((x+x^2-cr^2)*vx + y*vy)) / (y^2+(x+x^2-cr^2)^2) )
#       (y*(vx*x+vy*y)) / (x^2+y^2) )
       2*vx*x + 2*vy*y )
    goto L then 

    x,y,
#	((y^2+c*(x+x^2-cr^2)^2)*vx + (c-1)*y*(x+x^2-cr^2)*vy) / (y^2+(x+x^2-cr^2)^2),
#	((c-1)*y*(x+x^2-cr^2)*vx + (c*y^2+(x+x^2-cr^2)^2)*vy) / (y^2+(x+x^2-cr^2)^2)
	(c*vx*x^2 + (c-1)*vy*x*y+vx*y^2) / (x^2+y^2),
	((c-1)*vx*x*y + vy*(x^2+c*y^2)) / (x^2+y^2)
end

param order = 20
param t_max = 1000
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1
