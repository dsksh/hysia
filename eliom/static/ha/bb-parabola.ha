let pertb = 0

let g = 1
let f = 0

var   x,y,vx,vy

init  Loc, 1,2+pertb,0,0

at Loc wait vx,vy,0,-g

  once (x^2-y, vy-2*x*vx) goto Loc then 
    x,y,
    vx + (4*x*(vy - 2*x*vx)) / (4*x^2 + 1),
    vy - (2*(vy - 2*x*vx)) / (4*x^2 + 1)
end
