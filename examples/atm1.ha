let pi = 3.141592
let cx = 0
let cy = 0
let dist = 3
let rd = R 2
let th = R 1

let y10 = 0
(*let y10 = [0,0.1]*)

var   t, r, x1,y1,vx1,vy1, x2,y2,vx2,vy2

init  Start, 0, dist+rd, 10,0,-1,0, 10*cos(3+th),10*sin(3+th),-cos(3+th),-sin(3+th)

at Start wait 0, 0, vx1,vy1,0,0, vx2,vy2,0,0
  watch F ((x1-cx)^2+(y1-cy)^2-r^2*3, 2*x1*vx1 + 2*y1*vy1)
  goto Enter then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2
end

at Enter wait 1, 0, vx1,vy1,-vy1/r,vx1/r, vx2,vy2,-vy2/r,vx2/r
  (*watch F ((x1-cx)^2+(y1-cy)^2-r^2, true)*)
  watch F (pi*r/3 - t, true)
  goto Cycle then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2
end

at Cycle wait 1, 0, vx1,vy1,vy1/r,-vx1/r, vx2,vy2,vy2/r,-vx2/r
  watch F (pi*2*r/3 - t, true)
  goto Leave then 0,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2
end

at Leave wait 1, 0, vx1,vy1,-vy1/r,vx1/r, vx2,vy2,-vy2/r,vx2/r
  watch F (pi*r/3 - t, true)
  goto Start then t,r, x1,y1,vx1,vy1, x2,y2,vx2,vy2
end

(*prop G[0,30]( (x1-x2)^2+(y1-y2)^2 - 9 ) &
  F[0,10]( G[0,10]( !((x1-x2)^2+(y1-y2)^2 - 100) ) &
           F[10,20](  (x1-x2)^2+(y1-y2)^2 - 200 ) ) 
*)

prop G[0,30]( (x1-x2)^2+(y1-y2)^2 - 9 ) &
  F[0,10]( G[0,10]( !((x1-cx)^2+(y1-cx)^2 - 25) ) &
           F[10,20](  (x1-cx)^2+(y1-cx)^2 - 100 ) ) &
  F[0,10]( G[0,10]( !((x2-cx)^2+(y2-cx)^2 - 25) ) &
           F[10,20](  (x2-cx)^2+(y2-cx)^2 - 100 ) )

param order = 20
param t_max = 20
#param h_min   = 0.1
param h_min   = 1e-14
param epsilon = 1e-14
#param epsilon = 0
param dump_interval = 0.1
#param delta = 0.99
#param tau = 1.01
#param char_mtx = -1
#param char_mtx = 1
#param char_mtx = 2
param char_mtx = 3
param qr_thres = 8000
