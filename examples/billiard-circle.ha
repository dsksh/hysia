let g = 100
let c = 1
let cr = 5

var   x,y,vx,vy

init  L, 0,0.5, 1,1

at L wait vx, vy, 0, 0
  once (
		x^2+y^2-cr^2, 
        -2*vx*x - 2*vy*y )
    goto L then 

    x,y,
	(-c*vx*x^2 - (c+1)*vy*x*y + vx*y^2) / (x^2+y^2),
	(-(c+1)*vx*x*y + vy*(x^2 - c*y^2)) / (x^2+y^2)
end

param order = 10
param t_max = 50
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param delta= 100
param dump_interval = 0.1
#param cm_thres = 0
