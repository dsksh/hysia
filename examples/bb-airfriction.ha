let g = 1
let c = 1
#let c = 0.75
let f = 0.2
let y0 = 10

var   t, y, dy

init  Fall, 0, y0, 0

at Fall wait 1, dy, -g + f*dy^2
  once (y, -dy) goto Rise then t, y, -c*dy
end

at Rise wait 1, dy, -g - f*dy^2
  once (dy, g + f*dy^2) goto Fall then t, y, dy
end

param order = 20
param t_max = 20
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1
