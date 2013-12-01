var   x,y,dx,dy

init  L, 1,10,0,0

at L wait dx,dy,0,-10
  watch (x^2-y, dy-2*x*dx) goto L then 
    x,y,
    dx - 1.9 * ((2*x*dx - dy) / (4*x^2 +1)) * 2*x,
    dy + 1.9 * ((2*x*dx - dy) / (4*x^2 +1))
end

param order = 20
param t_max = 10
param h_max = 0.01
param dump_interval = 0.01
