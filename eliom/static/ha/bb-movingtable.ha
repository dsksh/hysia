let g = 1
let c = 0.9
let f = 0.04
let x0 = R 5

var   t, x, vx

init  Fall, 0, 2+x0, 0

at Fall wait 1, vx, -g + f*vx^2
  watch F (sin(t)-x, vx-cos(t), -c*vx + (c+1)*cos(t)) goto Fall then t, x, -c*vx + (c+1)*cos(t)
  watch F (sin(t)-x, vx-cos(t),  c*vx - (c+1)*cos(t)) goto Rise then t, x, -c*vx + (c+1)*cos(t)
end

at Rise wait 1, vx, -g - f*vx^2
  watch F (vx, true) goto Fall then t, x, vx
  watch F (sin(t)-x, vx-cos(t)) goto Rise then t, x, -c*vx + (c+1)*cos(t)
end


prop G[0,10](F[0,5](x-2))


param order = 30
param qr_thres = 8000
