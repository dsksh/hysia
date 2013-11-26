let Tr = 80
let Vr = 34
let V1 = 25
let V2 = 10
let Tmax = 1100
let Tmin = 250

var   t, x1, x2

init  No_rods, 0, Tr, Tr

at No_rods wait Vr, 1, 1
  watch (t-Tmax, x1-Tr) goto Rod1 then t, x1, x2
  watch (t-Tmax, x2-Tr) goto Rod2
  watch (t-Tmax, -(x1-Tr)*(x2-Tr)) goto Shutdown
end

at Rod1 wait -V1, 1, 1
  watch (t-Tmin, t) goto No_rods then t, 0, x2
end

at Rod2 wait -V2, 1, 1
  watch (t-Tmin, t) goto No_rods then t, x1, 0
end

at Shutdown wait 0, 0, 0 end

param order = 5
param t_end = 10
param h_max = 1
