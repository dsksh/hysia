let g = 1
#let g = 10
let c = 1
#let c = 1.25
#let c = 0.99
#let c = 0.9
#let c = 0.75
let coeff = 0.7071
let x0 = 0
let y0 = 10
let vx0 = 0
let vy0 = 0

var   x, y, vx, vy

init  L, x0, y0, vx0, vy0

at L wait vx, vy, 0, -g
  once (x + y, coeff*vx + coeff*vy) goto L
#  then x, y, -c*vy, -c*vx
  then x, y, (vx-vy-c*(vx+vy))/2, (-vx+vy-c*(vx+vy))/2
end

param order = 10
param t_max = 10
param h_min   = 1e-12
param epsilon = 1e-12
#param epsilon = 0
#param dump_interval = 0.01
param dump_interval = 0.1
