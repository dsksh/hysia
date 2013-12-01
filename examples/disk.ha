var   x1, x2

init  L, 1+[-1e-5,1e-5], [-1e-5,1e-5]
#init  L, 1, 0

at L wait -x2, x1
  watch ((x1-1)^2 + x2^2 - 1, -2*x2) goto L then -x1+2, -x2
end

param order = 10
param t_end = 10
param epsilon = 0.000000000000001
param h_min   = 0.000000000000001
param h_max = 0.1
