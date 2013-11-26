var   x1, x2, c

init  L, 1+[-1e-6,1e-6], [-1e-6,1e-6], 2

at L wait -c*x2, c*x1, 0
  watch (x1-x2, -c*x2) goto L then x1, x2, -c
end

param order = 20
param t_end = 10
param h_max = 0.1
