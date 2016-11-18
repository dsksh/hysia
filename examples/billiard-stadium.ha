#let w = 0.6
let w = 0.9

var   x1,x2,vx1,vx2

init  L, 0,0, w,1

at L wait vx1, vx2, 0, 0
  once ( x1^2 + 16*x2^4 - 1,
		 2*vx1*x1 + 64*vx2*x2^3 )
    goto L then 

    x1,x2,
	-(vx1*x1^2 + 64*vx2*x1*x2^3 - 1024*vx1*x2^6) / (x1^2 + 1024*x2^6),
	(vx2*x1^2 - 64*vx1*x1*x2^3 - 1024*vx2*x2^6) / (x1^2 + 1024*x2^6)
end

param order = 10
param t_max = 2
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.005
param cm_thres = 100
