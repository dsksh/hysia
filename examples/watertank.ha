let rateIn = 10
let rr = 2
let cc = 1
let high = 10
let low = 5
let delay = R 1

var   x, y

init  On, low, 0

at On wait (rateIn-x/rr)/cc, 0
  once (x-high, -x) goto Sw_off then x, 0
end

at Sw_off wait (rateIn-x/rr)/cc, 1
  once (y-delay, -y) goto Off then x, 0
end

at Off wait -(x/rr)/cc, 0
  once (x-low, -x) goto Sw_on then x, 0
end

at Sw_on wait -(x/rr)/cc, 1 
  once (y-delay, -y) goto On then x, 0 
end


param order = 20
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 10

param cm_thres = 0
