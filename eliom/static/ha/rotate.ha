let   intv = [-1e-6,1e-6]

let   c = 2
let   o = 0

var   x1, x2

init  Loc1, 1+intv, 0+intv

at Loc1 wait -x2, x1
  when (c*x1-x2+o, -x1) goto Loc2 then x1, x2
end

at Loc2 wait -x2, x1
  when (c*x1-x2+o, x1) goto Loc1 then x1, x2
end
