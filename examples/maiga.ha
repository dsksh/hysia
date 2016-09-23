let a1 = 1.5
let a2 = 3.5
let b1 = 1
let b2 = 3.5

var  x1, x2, t

init L1, 1.1, 0.1, 0

at L1 wait 1 - (b1+1)*x1 + a1*x1^2*x2, 
           b1*x1 - a1*x1^2*x2, 
           1
  once (-4*x1 + x2 + 2, -4*x1 + x2) goto L2 then x1, x2, t
end

at L2 wait 1 - (b2+1)*x1 + a2*x1^2*x2, 
           b2*x1 - a1*x1^2*x2, 
           1
end

param order = 50
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
#param delta = 0.9
#param tau   = 1.1
param dump_interval = 0.01
