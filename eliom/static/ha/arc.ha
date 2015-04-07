let   intv = [-1e-5,1e-5]

let   c = 1
let   o = 0

var   x1, x2, v

init  Loc, 1+intv, intv, 1

at Loc wait -v*x2, v*x1, 0
  watch (c*x1-x2+o, -v*x1) goto Loc then x1, x2, -v
end
