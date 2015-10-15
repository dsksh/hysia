let pi = 3.141592
let cx = 0
let cy = 0
let dist = 3
let th = 1.570796
let thr1 = R 0.785398
let thr2 = R 0.785398
let thr3 = R 0.785398
let thr4 = R 0.785398

let rd = R 2

var   t, r, x1,y1,vx1,vy1, x2,y2,vx2,vy2, x3,y3,vx3,vy3, x4,y4,vx4,vy4

init  Start, 0, dist+rd, 10*cos(th*0-th/4+thr1),10*sin(th*0-th/4+thr1),-cos(th*0-th/4+thr1),-sin(th*0-th/4+thr1), 10*cos(th*1-th/4+thr2),10*sin(th*1-th/4+thr2),-cos(th*1-th/4+thr2),-sin(th*1-th/4+thr2), 10*cos(th*2-th/4+thr3),10*sin(th*2-th/4+thr3),-cos(th*2-th/4+thr3),-sin(th*2-th/4+thr3), 10*cos(th*3-th/4+thr4),10*sin(th*3-th/4+thr4),-cos(th*3-th/4+thr4),-sin(th*3-th/4+thr4)

at Start wait 0, 0, vx1,vy1,0,0, vx2,vy2,0,0, vx3,vy3,0,0, vx4,vy4,0,0
  once ((x1-cx)^2+(y1-cy)^2-r^2*3, 2*x1*vx1 + 2*y1*vy1)
  goto Enter then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2, x3,y3,vx3,vy3, x4,y4,vx4,vy4
end

at Enter wait 1, 0, vx1,vy1,-vy1/r,vx1/r, vx2,vy2,-vy2/r,vx2/r, vx3,vy3,-vy3/r,vx3/r, vx4,vy4,-vy4/r,vx4/r
  once (pi*r/3 - t, true)
  goto Cycle then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2, x3,y3,vx3,vy3, x4,y4,vx4,vy4
end

at Cycle wait 1, 0, vx1,vy1,vy1/r,-vx1/r, vx2,vy2,vy2/r,-vx2/r, vx3,vy3,vy3/r,-vx3/r, vx4,vy4,vy4/r,-vx4/r
  once (pi*2*r/3 - t, true)
  goto Leave then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2, x3,y3,vx3,vy3, x4,y4,vx4,vy4
end

at Leave wait 1, 0, vx1,vy1,-vy1/r,vx1/r, vx2,vy2,-vy2/r,vx2/r, vx3,vy3,-vy3/r,vx3/r, vx4,vy4,-vy4/r,vx4/r
  once (pi*r/3 - t, true)
  goto Start then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2, x3,y3,vx3,vy3, x4,y4,vx4,vy4
end


(* BLTL property *)

prop 
G[0,30]( ((x1-x2)^2+(y1-y2)^2) - 4 ) &
G[0,30]( ((x1-x3)^2+(y1-y3)^2) - 4 ) &
G[0,30]( ((x1-x4)^2+(y1-y4)^2) - 4 ) &
G[0,30]( ((x2-x3)^2+(y2-y3)^2) - 4 ) &
G[0,30]( ((x2-x4)^2+(y2-y4)^2) - 4 ) &
G[0,30]( ((x3-x4)^2+(y3-y4)^2) - 4 ) &


F[0,10]( G[0,10]( !((x1-cx)^2+(y1-cy)^2 -  25) ) & F[10,20](  (x1-cx)^2+(y1-cy)^2 - 100) ) &
F[0,10]( G[0,10]( !((x2-cx)^2+(y2-cy)^2 -  25) ) & F[10,20](  (x2-cx)^2+(y2-cy)^2 - 100) ) &
F[0,10]( G[0,10]( !((x3-cx)^2+(y3-cy)^2 -  25) ) & F[10,20](  (x3-cx)^2+(y3-cy)^2 - 100) ) &
F[0,10]( G[0,10]( !((x4-cx)^2+(y4-cy)^2 -  25) ) & F[10,20](  (x4-cx)^2+(y4-cy)^2 - 100) )


(* solving parameters *)

param t_max = 20
param dump_interval = 0.6
