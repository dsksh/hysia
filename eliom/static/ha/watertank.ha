let rateIn = 10
let rr = 2
let cc = 1
let high = 10
let low = 5
let delay = R 1

var   x, y

init  On, low, 0

at On wait (rateIn-x/rr)/cc, 0
  watch (x-high, -x) goto Sw_off then x, 0
end

at Sw_off wait (rateIn-x/rr)/cc, 1
  watch (y-delay, -y) goto Off then x, 0
end

at Off wait -(x/rr)/cc, 0
  watch (x-low, -x) goto Sw_on then x, 0
end

at Sw_on wait -(x/rr)/cc, 1 
  watch (y-delay, -y) goto On then x, 0 
end


(* solving parameters *)

param cm_thres = 0
