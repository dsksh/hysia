let RIn = 10
let R = 2
let C = 1
let High = 10
let Low = 5
let Delay = 1

var   x, y

init  On, Low, 0

at On wait (RIn-x/R)/C, 0
  watch (x-High, -x) goto Sw_off then x, 0
end

at Sw_off wait (RIn-x/R)/C, 1
  watch (y-Delay, -y) goto Off then x, 0
end

at Off wait -(x/R)/C, 0
  watch (x-Low, -x) goto Sw_on then x, 0
end

at Sw_on wait -(x/R)/C, 1 
  watch (y-Delay, -y) goto On then x, 0 
end


param order = 10
param t_max = 100
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 10
param char_mtx = 3
