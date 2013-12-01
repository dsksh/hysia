let g = 10
let c = 0.75

let y0 = 10
var   t, y, dy


init  L, 0, 10, 0

at L wait 1, dy, -10
  watch (y, dy) goto L then t, y, -dy
end

param order = 20
param t_max = 10
param h_min   = 0.00000000000001
param epsilon = 0.00000000000001
param dump_interval = 0.2
