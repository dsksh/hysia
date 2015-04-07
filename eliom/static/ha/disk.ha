let intv = [-1e-6,1e-6]

var   x1, x2

init  Loc, 1+intv, intv

at Loc wait -x2, x1
  watch ((x1-1)^2 + x2^2 - 1, -2*x2) goto Loc then -x1+2, -x2
end
