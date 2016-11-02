var  x1, x2, t

init L1, -0.9, 3.1, 0
#init L1, [-0.9,-0.8], [3.0,3.1], 0
#init L1, [-0.9,-0.8], [3.0,3.5], 0

at L1 wait x2, -6*x2 - 10*sin(x1), 1
  once (cos(x1) - x2/10 - 0.7, -x2) goto L2 
  #then -x1, -2*x2, t
  then -x1, 2*x2, t
end

at L2 wait x2, -18*x2 - 10*sin(x1), 1
  #once (t-0.5, -t) goto L2 then 0, 0, 0
  once (cos(x1) - x2/10 - 0.7, -x2) goto L1 
  then -x1, 2*x2, t
end

param order = 30
param t_end = 10
param h_min   = 1e-14
param epsilon = 1e-14
param delta = 0.9
param tau   = 1.1
param dump_interval = 0.005
