let g = 100
let c = 1
let cr = 30

var   x,y,vx,vy

init  L, 0,40,1,0
#init  L, 0,30,1,0

at L wait vx, vy, -g*x/sqrt(x^2+y^2)^3, -g*y/sqrt(x^2+y^2)^3
  once (
		x^2+y^2-cr^2, 
        2*vx*x + 2*vy*y )
#       vy )
    goto L then 

    x,y,
	(-c*vx*x^2 - (c+1)*vy*x*y + vx*y^2) / (x^2+y^2),
	(-(c+1)*vx*x*y + vy*(x^2 - c*y^2)) / (x^2+y^2)
#	-vx,-vy
end

param order = 20
param t_max = 50
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 1
