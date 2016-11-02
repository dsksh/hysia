let pi = 3.141592
let cx = 0
let cy = 0
let dist = 3
let th1 = R 1.59 (* 2.09 - dist/10 *)
#let th1 = 1.8
let th2 = R 1.59
#let th2 = 0
let th3 = R 1.59
let rd = R 2
#let rd = 2

var   t, r, x1,y1,vx1,vy1,
            x2,y2,vx2,vy2,
            x3,y3,vx3,vy3

init  Start, 0, dist+rd, 
             10*cos(2.09*0+th1),10*sin(2.09*0+th1),-cos(2.09*0+th1),-sin(2.09*0+th1),
             10*cos(2.09*1+th2),10*sin(2.09*1+th2),-cos(2.09*1+th2),-sin(2.09*1+th2),
             10*cos(2.09*2+th3),10*sin(2.09*2+th3),-cos(2.09*2+th3),-sin(2.09*2+th3)

at Start wait 0, 0, vx1,vy1,0,0, 
                    vx2,vy2,0,0,
                    vx3,vy3,0,0
  once ((x1-cx)^2+(y1-cy)^2-r^2*3, 2*x1*vx1 + 2*y1*vy1)
  goto Enter then 0,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3
end

at Enter wait 1, 0, vx1,vy1,-vy1/r,vx1/r,
                    vx2,vy2,-vy2/r,vx2/r,
                    vx3,vy3,-vy3/r,vx3/r
  once (pi*r/3 - t, true)
  goto Cycle then 0,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3
end

at Cycle wait 1, 0, vx1,vy1,vy1/r,-vx1/r,
                    vx2,vy2,vy2/r,-vx2/r,
                    vx3,vy3,vy3/r,-vx3/r
  once (pi*2*r/3 - t, true)
  goto Leave then 0,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3
end

at Leave wait 1, 0, vx1,vy1,-vy1/r,vx1/r,
                    vx2,vy2,-vy2/r,vx2/r,
                    vx3,vy3,-vy3/r,vx3/r
  once (pi*r/3 - t, true)
  goto Start then t,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3
end

(*prop 
  G[0,30]( ((x1-x2)^2+(y1-y2)^2) - 9 ) &
  G[0,30]( ((x1-x3)^2+(y1-y3)^2) - 9 ) &
  G[0,30]( ((x2-x3)^2+(y2-y3)^2) - 9 ) &

  (*F[0,10]( G[0,10]( !((x1-x2)^2+(y1-y2)^2 - 100) ) &
           F[10,20](  (x1-x2)^2+(y1-y2)^2 - 200) ) &

  F[0,10]( G[0,10]( !((x1-x3)^2+(y1-y3)^2 - 100) ) &
           F[10,20](  (x1-x3)^2+(y1-y3)^2 - 200) ) &

  F[0,10]( G[0,10]( !((x2-x3)^2+(y2-y3)^2 - 100) ) &
           F[10,20](  (x2-x3)^2+(y2-y3)^2 - 200) )
  *)

  F[0,10]( G[0,10]( !((x1-cx)^2+(y1-cy)^2 -  25) ) &
           F[10,20](  (x1-cx)^2+(y1-cy)^2 - 100) ) &

  F[0,10]( G[0,10]( !((x2-cx)^2+(y2-cy)^2 -  25) ) &
           F[10,20](  (x2-cx)^2+(y2-cy)^2 - 100) ) &

  F[0,10]( G[0,10]( !((x3-cx)^2+(y3-cy)^2 -  25) ) &
           F[10,20](  (x3-cx)^2+(y3-cy)^2 - 100) )
*)


param order = 20
param t_max = 100
#param h_min   = 0.1
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.2
param cm_thres = 0
