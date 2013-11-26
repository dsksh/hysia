var   t, y, dy

init  L, 0, 10, 0

at L wait 1, dy, -10
  watch (y, dy) goto L then t, y, -0.5*dy
end

param order = 20
param t_end = 10
param h_max = 0.1
