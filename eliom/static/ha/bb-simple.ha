let pertb = [-1e-11,1e-11]

let g = 1
let c = 1
let f = 0

var   y, vy

init  Loc, 1+pertb, -0+pertb

at Loc wait vy, -g
  watch F (y, vy) goto Loc then y, -c*vy
end


(* solving parameters *)

param order = 20
param qr_thres = 15000
