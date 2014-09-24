let Tr = 80
let TrI = 50
#let TrI = [50,60]
let Vr = 34
let V1 = 25
let V2 = 10
let Tmax = 1100
#let Tmax = [1100,1200]
let Tmin = 250

let intv = [-2,2]

var   th, x1, x2

#init  No_rods, 0, Tr, 0
init  No_rods, 0+intv, Tr+intv, 0+intv

at No_rods wait Vr, 1, 1
  invariant Tmax-th

  watch (Tmax-th, Tr-x1) goto Rod1 then th, x1, x2
  watch (Tmax-th, Tr-x2) goto Rod2 then th, x1, x2
  watch (Tmax-th, -(x1-Tr)*(x2-Tr)) goto Shutdown then 0, 0, 0
end

at Rod1 wait -V1, 1, 1
  invariant th-Tmin

  watch (th-Tmin, -th) goto No_rods then th, 0, x2
end

at Rod2 wait -V2, 1, 1
  invariant th-Tmin

  watch (th-Tmin, -th) goto No_rods then th, x1, 0
end

at Shutdown wait 0, 0, 0 end

param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
#param char_mtx = -1
#param char_mtx = 0
param char_mtx = 3
param dump_interval = 1
#param dump_interval = -1
