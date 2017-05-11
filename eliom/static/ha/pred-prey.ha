let g1 = 0.2
let g2 = 0.1

let c = 0.5

var   py, pd

init  L2, 7, 3 

at L1 wait	 g2*py - 0.04*py*pd,
			-g1*pd + 0.02*py*pd 
			
  once (pd - c*py, 
		pd*(g1-0.04*py) + c*g2*py)
    goto L2 then py, pd
end

at L2 wait	 g1*py - 0.04*py*pd, 
			-g2*pd + 0.02*py*pd
  once (pd - py/2, 
		-(pd*(g2-0.04*py) + c*g1*py))
    goto L1 then py, pd
end

param order = 20
