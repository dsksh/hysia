var   x1, x2

init  L, 1+[-1e-6,1e-6], [-1e-6,1e-6]

at L wait -x2, x1
  watch ((x1-1)^2 + x2^2 - 1, -2*x2) goto L then -x1+2, -x2
end

param order = 20
param t_end = 10
param h_max = 0.1
