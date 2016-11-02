let g = 1
let c = 1
let cr = 3

var   x,y,z,vx,vy,vz

#init  L, 0,0,4,0.1,0,0
#init  L, 0,0,3.5,0.1,0,0
init  L, 0,0,3.4,0.1,0,0
#init  L, 0,0,3.2,0.1,0,0
#init  L, 0,0,3.1,0.1,0,0

at L wait vx, vy, vz, 
          -g*x/sqrt(x^2+y^2+z^2)^3, -g*y/sqrt(x^2+y^2+z^2)^3, -g*z/sqrt(x^2+y^2+z^2)^3
  once (
		x^2+y^2+z^2
        #-(x*y*z)^2
        -cr^2, 
        2*vx*x + 2*vy*y + 2*vz*z )
    goto L then 

    x,y,z,
	-(vy*x*y + vz*x*z + c*x*(vx*x + vy*y + vz*z) - vx*(y^2+z^2)) / (x^2+y^2+z^2),
	(-(1+c)*y*(vx*x + vz*z) + vy*(x^2 - c*y^2 + z^2)) / (x^2+y^2+z^2),
	(-(1+c)*(vx*x + vy*y)*z + vz*(x^2 + y^2 - c*z^2)) / (x^2+y^2+z^2)

#	(-2*x*(vy*y + vz*z) + vx*(-x^2 + y^2 + z^2)) / (x^2+y^2+z^2),
#	(-2*y*(vx*x + vz*z) + vy*( x^2 - y^2 + z^2)) / (x^2+y^2+z^2),
#	(-2*z*(vx*x + vy*y) + vz*( x^2 + y^2 - z^2)) / (x^2+y^2+z^2)
end

param order = 30
param t_max = 50
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 1
