let pi = 3.141592
let cx = 0
let cy = 0
let dist = 3
let th1 = R 0.75 (* 1.25 - dist/10 *)
let th2 = R 0.75
let th3 = R 0.75
let th4 = R 0.75
let th5 = R 0.75
let rd = R 2

var   t, r, x1,y1,vx1,vy1,
            x2,y2,vx2,vy2,
            x3,y3,vx3,vy3,
            x4,y4,vx4,vy4,
            x5,y5,vx5,vy5

init  Start, 0, dist+rd, 
             10*cos(1.25*0+th1),10*sin(1.25*0+th1),-cos(1.25*0+th1),-sin(1.25*0+th1),
             10*cos(1.25*1+th2),10*sin(1.25*1+th2),-cos(1.25*1+th2),-sin(1.25*1+th2),
             10*cos(1.25*2+th3),10*sin(1.25*2+th3),-cos(1.25*2+th3),-sin(1.25*2+th3),
             10*cos(1.25*3+th4),10*sin(1.25*3+th4),-cos(1.25*3+th4),-sin(1.25*3+th4),
             10*cos(1.25*4+th5),10*sin(1.25*4+th5),-cos(1.25*4+th5),-sin(1.25*4+th5)

at Start wait 0, 0, vx1,vy1,0,0, 
                    vx2,vy2,0,0,
                    vx3,vy3,0,0,
                    vx4,vy4,0,0,
                    vx5,vy5,0,0
  once ((x1-cx)^2+(y1-cy)^2-r^2*3, -2*x1*vx1 - 2*y1*vy1)
  goto Enter then 0,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3,
                       x4,y4,vx4,vy4,
                       x5,y5,vx5,vy5
end

at Enter wait 1, 0, vx1,vy1,-vy1/r,vx1/r,
                    vx2,vy2,-vy2/r,vx2/r,
                    vx3,vy3,-vy3/r,vx3/r,
                    vx4,vy4,-vy4/r,vx4/r,
                    vx5,vy5,-vy5/r,vx5/r
  once (pi*r/3 - t, true)
  goto Cycle then 0,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3,
                       x4,y4,vx4,vy4,
                       x5,y5,vx5,vy5
end

at Cycle wait 1, 0, vx1,vy1,vy1/r,-vx1/r,
                    vx2,vy2,vy2/r,-vx2/r,
                    vx3,vy3,vy3/r,-vx3/r,
                    vx4,vy4,vy4/r,-vx4/r,
                    vx5,vy5,vy5/r,-vx5/r
  once (pi*2*r/3 - t, true)
  goto Leave then 0,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3,
                       x4,y4,vx4,vy4,
                       x5,y5,vx5,vy5
end

at Leave wait 1, 0, vx1,vy1,-vy1/r,vx1/r,
                    vx2,vy2,-vy2/r,vx2/r,
                    vx3,vy3,-vy3/r,vx3/r,
                    vx4,vy4,-vy4/r,vx4/r,
                    vx5,vy5,-vy5/r,vx5/r
  once (pi*r/3 - t, true)
  goto Start then t,r, x1,y1,vx1,vy1,
                       x2,y2,vx2,vy2,
                       x3,y3,vx3,vy3,
                       x4,y4,vx4,vy4,
                       x5,y5,vx5,vy5
end

prop
  G[0,20] 400 - (x1-x2)^2+(y1-y2)^2 &
  G[0,20] 400 - (x1-x3)^2+(y1-y3)^2 &
  G[0,20] 400 - (x1-x4)^2+(y1-y4)^2 &
  G[0,20] 400 - (x1-x5)^2+(y1-y5)^2 &

  G[0,20] 400 - (x2-x3)^2+(y2-y3)^2 &
  G[0,20] 400 - (x2-x4)^2+(y2-y4)^2 &
  G[0,20] 400 - (x2-x5)^2+(y2-y5)^2 &

  G[0,20] 400 - (x3-x4)^2+(y3-y4)^2 &
  G[0,20] 400 - (x3-x5)^2+(y3-y5)^2 &

  G[0,20] 400 - (x4-x5)^2+(y4-y5)^2 &

  F[0,10]( G[ 0,10] ! (x1-cx)^2+(y1-cy)^2 - 25 &
           F[10,20] 100 - (x1-cx)^2+(y1-cy)^2  ) &

  F[0,10]( G[ 0,10] ! (x2-cx)^2+(y2-cy)^2 - 25 &
           F[10,20] 100 - (x2-cx)^2+(y2-cy)^2  ) &

  F[0,10]( G[ 0,10] ! (x3-cx)^2+(y3-cy)^2 - 25 &
           F[10,20] 100 - (x3-cx)^2+(y3-cy)^2  ) &

  F[0,10]( G[ 0,10] ! (x4-cx)^2+(y4-cy)^2 - 25 &
           F[10,20] 100 - (x4-cx)^2+(y4-cy)^2  ) &

  F[0,10]( G[ 0,10] ! (x5-cx)^2+(y5-cy)^2 - 25 &
           F[10,20] 100 - (x5-cx)^2+(y5-cy)^2  )

param order = 20
param t_max = 20
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1
param cm_thres = 0
