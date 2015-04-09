let g = 1
let c = 0.9
let f = 0.04
let x0 = R 5

var   x, vx, t

init  Fall, 2+x0, 0, 0

at Fall wait vx, -g + f*vx^2, 1
  watch F (sin(t)-x, vx-cos(t), -c*vx + (c+1)*cos(t)) goto Fall then x, -c*vx + (c+1)*cos(t), t
  watch F (sin(t)-x, vx-cos(t),  c*vx - (c+1)*cos(t)) goto Rise then x, -c*vx + (c+1)*cos(t), t
end

at Rise wait vx, -g - f*vx^2, 1
  watch F (vx, true) goto Fall then x, vx, t
  watch F (sin(t)-x, vx-cos(t)) goto Rise then x, -c*vx + (c+1)*cos(t), t
end


(* MITL property to satisfy *)

prop G[0,10](F[0,5](x-2))


(* solving parameters *)

param order = 30
param qr_thres = 8000
