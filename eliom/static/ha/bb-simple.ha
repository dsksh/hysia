let pertb = [-1e-11,1e-11]

let g = 1
let c = 1
let f = 0

var   t, y, vy

init  Loc, 0, 1+pertb, -0+pertb

at Loc wait 1, vy, -g
  watch F (y, vy) goto Loc then t, y, -c*vy
end


param qr_thres = 10000
