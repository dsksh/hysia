let g = 1
#let g = 10
let c = 1
#let c = 0.9

#let pertb = 0
#let pertb = [-1e-1,1e-1]
#let pertb = [-1e-3,1e-3]
#let pertb = [-1e-4,1e-4]
let pertb = [-1e-5,1e-5]
#let pertb = [-1e-11,1e-11]

var   t, y, vy

init  Loc, 0, 1+pertb, -pertb

at Loc wait 1, vy, -g
  once (y, -vy) goto Loc then t, y, -c*vy
end

param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1

# cm_thres:: 1: QR,  -1: inverse,  0: ident,  >1: hybrid
#param cm_thres = 100
#param cm_thres = 10
#param cm_thres = 1
#param cm_thres = -1
#param cm_thres = 0
