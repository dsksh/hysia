let g = 1
let c = 0.9
let f = 0.05
let x0 = R 5

var   x, vx, t

init  Fall, 2+x0, 0, 0

at Fall wait vx, -g + f*vx^2, 1
  once (sin(t)-x, cos(t)-vx,  c*vx - (c+1)*cos(t))
       goto Fall then x, -c*vx + (c+1)*cos(t), t
  once (sin(t)-x, cos(t)-vx, -c*vx + (c+1)*cos(t))
       goto Rise then x, -c*vx + (c+1)*cos(t), t
end

at Rise wait vx, -g - f*vx^2, 1
  once (vx, true) 
       goto Fall then x, vx, t
  once (sin(t)-x, cos(t)-vx) 
       goto Rise then x, -c*vx + (c+1)*cos(t), t
end


(* STL property *)

prop G[0,10] F[0,5] x-2


(* solving parameters *)

param order = 20
param epsilon = 1e-12
