let Pi = 3.141592
#let A1_1 = -0.8
#let A1_2 = -0.2
#let A2_1 = -0.1
#let A2_2 = -0.8
let A1_1 = -1.2
let A1_2 = 0.1
let A2_1 = 0.1
let A2_2 = -1.2

#let I1_1 = 2
#let I1_2 = 4
#let I1_3 = 6
#let I1_4 = 6
#let I1_5 = 6
#let I2_1 = 2
#let I2_2 = 4
#let I2_3 = 7
#let I2_4 = 7
#let I2_5 = 4
#let I3_1 = 2
#let I3_2 = 4
#let I3_3 = 0
#let I3_4 = 3
#let I3_5 = 4
#let I4_1 = 2
#let I4_2 = 4
#let I4_3 = 6
#let I4_4 = 6
#let I4_5 = 6
#let I5_1 = 2
#let I5_2 = 0
#let I5_3 = 0
#let I5_4 = 0
#let I5_5 = 0

let I1_1 = 4
let I1_2 = 6
let I1_3 = 6
let I1_4 = 6
let I1_5 = 4
let I2_1 = 4
let I2_2 = 0
let I2_3 = 1
let I2_4 = 1
let I2_5 = 4
let I3_1 = 4
let I3_2 = 2
let I3_3 = 1
let I3_4 = 1
let I3_5 = 4
let I4_1 = 3
let I4_2 = 0
let I4_3 = 0
let I4_4 = 0
let I4_5 = 4
let I5_1 = 3
let I5_2 = 0
let I5_3 = 0
let I5_4 = 6
let I5_5 = 6

let vx0 = R 2
#let vx0 = [0.999,1]
let vy0 = R 2

var   x1, x2, v1, v2

init  L2_4, 3.5, 3.5, vx0-1, vy0-1

at L1_1 wait v1, v2, 
             A1_1*(v1-sin(I1_1*Pi/4)) + A1_2*(v2-cos(I1_1*Pi/4)), 
             A2_1*(v1-sin(I1_1*Pi/4)) + A2_2*(v2-cos(I1_1*Pi/4))

  once (1-x1,  v1) goto L1_2 then x1, x2, v1, v2

  once (x2-4, -v2) goto L2_1 then x1, x2, v1, v2
end

at L1_2 wait v1, v2, 
             A1_1*(v1-sin(I1_2*Pi/4)) + A1_2*(v2-cos(I1_2*Pi/4)), 
             A2_1*(v1-sin(I1_2*Pi/4)) + A2_2*(v2-cos(I1_2*Pi/4))

  once (x1-1, -v1) goto L1_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L1_3 then x1, x2, v1, v2

  once (x2-4, -v2) goto L2_2 then x1, x2, v1, v2
end

at L1_3 wait v1, v2, 
             A1_1*(v1-sin(I1_3*Pi/4)) + A1_2*(v2-cos(I1_3*Pi/4)), 
             A2_1*(v1-sin(I1_3*Pi/4)) + A2_2*(v2-cos(I1_3*Pi/4))

  once (x1-2, -v1) goto L1_2 then x1, x2, v1, v2
  once (3-x1,  v1) goto L1_4 then x1, x2, v1, v2

  once (x2-4, -v2) goto L2_3 then x1, x2, v1, v2
end

at L1_4 wait v1, v2, 
             A1_1*(v1-sin(I1_4*Pi/4)) + A1_2*(v2-cos(I1_4*Pi/4)), 
             A2_1*(v1-sin(I1_4*Pi/4)) + A2_2*(v2-cos(I1_4*Pi/4))

  once (x1-3, -v1) goto L1_3 then x1, x2, v1, v2
  once (4-x1,  v1) goto L1_5 then x1, x2, v1, v2

  once (x2-4, -v2) goto L2_4 then x1, x2, v1, v2
end

at L1_5 wait v1, v2, 
             A1_1*(v1-sin(I1_5*Pi/4)) + A1_2*(v2-cos(I1_5*Pi/4)), 
             A2_1*(v1-sin(I1_5*Pi/4)) + A2_2*(v2-cos(I1_5*Pi/4))

  once (x1-4, -v1) goto L1_4 then x1, x2, v1, v2

  once (x2-4,  v2) goto L2_5 then x1, x2, v1, v2
end

at L2_1 wait v1, v2, 
             A1_1*(v1-sin(I2_1*Pi/4)) + A1_2*(v2-cos(I2_1*Pi/4)), 
             A2_1*(v1-sin(I2_1*Pi/4)) + A2_2*(v2-cos(I2_1*Pi/4))

  once (1-x1,  v1) goto L2_2 then x1, x2, v1, v2

  once (4-x2,  v2) goto L1_1 then x1, x2, v1, v2
  once (x2-3, -v2) goto L3_1 then x1, x2, v1, v2
end

at L2_2 wait v1, v2, 
             A1_1*(v1-sin(I2_2*Pi/4)) + A1_2*(v2-cos(I2_2*Pi/4)), 
             A2_1*(v1-sin(I2_2*Pi/4)) + A2_2*(v2-cos(I2_2*Pi/4))

  once (x1-1, -v1) goto L2_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L2_3 then x1, x2, v1, v2

  once (4-x2,  v2) goto L1_2 then x1, x2, v1, v2
  once (x2-3, -v2) goto L3_2 then x1, x2, v1, v2
end

at L2_3 wait v1, v2, 
             A1_1*(v1-sin(I2_3*Pi/4)) + A1_2*(v2-cos(I2_3*Pi/4)), 
             A2_1*(v1-sin(I2_3*Pi/4)) + A2_2*(v2-cos(I2_3*Pi/4))

  once (x1-2, -v1) goto L2_2 then x1, x2, v1, v2
  once (3-x1,  v1) goto L2_4 then x1, x2, v1, v2

  once (4-x2,  v2) goto L1_3 then x1, x2, v1, v2
  once (x2-3, -v2) goto L3_3 then x1, x2, v1, v2
end

at L2_4 wait v1, v2, 
             A1_1*(v1-sin(I2_4*Pi/4)) + A1_2*(v2-cos(I2_4*Pi/4)), 
             A2_1*(v1-sin(I2_4*Pi/4)) + A2_2*(v2-cos(I2_4*Pi/4))

  once (x1-3, -v1) goto L2_3 then x1, x2, v1, v2
  once (4-x1,  v1) goto L2_5 then x1, x2, v1, v2

  once (4-x2,  v2) goto L1_4 then x1, x2, v1, v2
  once (x2-3, -v2) goto L3_4 then x1, x2, v1, v2
end

at L2_5 wait v1, v2, 
             A1_1*(v1-sin(I2_5*Pi/4)) + A1_2*(v2-cos(I2_5*Pi/4)), 
             A2_1*(v1-sin(I2_5*Pi/4)) + A2_2*(v2-cos(I2_5*Pi/4))

  once (x1-4, -v1) goto L2_4 then x1, x2, v1, v2

  once (4-x2,  v2) goto L1_5 then x1, x2, v1, v2
  once (x2-3, -v2) goto L3_5 then x1, x2, v1, v2
end

at L3_1 wait v1, v2, 
             A1_1*(v1-sin(I3_1*Pi/4)) + A1_2*(v2-cos(I3_1*Pi/4)), 
             A2_1*(v1-sin(I3_1*Pi/4)) + A2_2*(v2-cos(I3_1*Pi/4))

  once (1-x1,  v1) goto L3_2 then x1, x2, v1, v2

  once (3-x2,  v2) goto L2_1 then x1, x2, v1, v2
  once (x2-2, -v2) goto L4_1 then x1, x2, v1, v2
end

at L3_2 wait v1, v2, 
             A1_1*(v1-sin(I3_2*Pi/4)) + A1_2*(v2-cos(I3_2*Pi/4)), 
             A2_1*(v1-sin(I3_2*Pi/4)) + A2_2*(v2-cos(I3_2*Pi/4))

  once (x1-1, -v1) goto L3_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L3_3 then x1, x2, v1, v2

  once (3-x2,  v2) goto L2_2 then x1, x2, v1, v2
  once (x2-2, -v2) goto L4_2 then x1, x2, v1, v2
end

at L3_3 wait v1, v2, 
             A1_1*(v1-sin(I3_3*Pi/4)) + A1_2*(v2-cos(I3_3*Pi/4)), 
             A2_1*(v1-sin(I3_3*Pi/4)) + A2_2*(v2-cos(I3_3*Pi/4))

  once (x1-2, -v1) goto L3_2 then x1, x2, v1, v2
  once (3-x1,  v1) goto L3_4 then x1, x2, v1, v2

  once (3-x2,  v2) goto L2_3 then x1, x2, v1, v2
  once (x2-2, -v2) goto L4_3 then x1, x2, v1, v2
end

at L3_4 wait v1, v2, 
             A1_1*(v1-sin(I3_4*Pi/4)) + A1_2*(v2-cos(I3_4*Pi/4)), 
             A2_1*(v1-sin(I3_4*Pi/4)) + A2_2*(v2-cos(I3_4*Pi/4))

  once (x1-3, -v1) goto L3_3 then x1, x2, v1, v2
  once (4-x1,  v1) goto L3_5 then x1, x2, v1, v2

  once (3-x2,  v2) goto L2_4 then x1, x2, v1, v2
  once (x2-2, -v2) goto L4_4 then x1, x2, v1, v2
end

at L3_5 wait v1, v2, 
             A1_1*(v1-sin(I3_5*Pi/4)) + A1_2*(v2-cos(I3_5*Pi/4)), 
             A2_1*(v1-sin(I3_5*Pi/4)) + A2_2*(v2-cos(I3_5*Pi/4))

  once (x1-4, -v1) goto L3_4 then x1, x2, v1, v2

  once (3-x2,  v2) goto L2_5 then x1, x2, v1, v2
  once (x2-2, -v2) goto L4_5 then x1, x2, v1, v2
end

at L4_1 wait v1, v2, 
             A1_1*(v1-sin(I4_1*Pi/4)) + A1_2*(v2-cos(I4_1*Pi/4)), 
             A2_1*(v1-sin(I4_1*Pi/4)) + A2_2*(v2-cos(I4_1*Pi/4))

  once (1-x1,  v1) goto L4_2 then x1, x2, v1, v2

  once (2-x2,  v2) goto L3_1 then x1, x2, v1, v2
  once (x2-1, -v2) goto L5_1 then x1, x2, v1, v2
end

at L4_2 wait v1, v2, 
             A1_1*(v1-sin(I4_2*Pi/4)) + A1_2*(v2-cos(I4_2*Pi/4)), 
             A2_1*(v1-sin(I4_2*Pi/4)) + A2_2*(v2-cos(I4_2*Pi/4))

  once (x1-1, -v1) goto L4_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L4_3 then x1, x2, v1, v2

  once (2-x2,  v2) goto L3_2 then x1, x2, v1, v2
  once (x2-1, -v2) goto L5_2 then x1, x2, v1, v2
end

at L4_3 wait v1, v2, 
             A1_1*(v1-sin(I4_3*Pi/4)) + A1_2*(v2-cos(I4_3*Pi/4)), 
             A2_1*(v1-sin(I4_3*Pi/4)) + A2_2*(v2-cos(I4_3*Pi/4))

  once (x1-2, -v1) goto L4_2 then x1, x2, v1, v2
  once (3-x1,  v1) goto L4_4 then x1, x2, v1, v2

  once (2-x2,  v2) goto L3_3 then x1, x2, v1, v2
  once (x2-1, -v2) goto L5_3 then x1, x2, v1, v2
end

at L4_4 wait v1, v2, 
             A1_1*(v1-sin(I4_4*Pi/4)) + A1_2*(v2-cos(I4_4*Pi/4)), 
             A2_1*(v1-sin(I4_4*Pi/4)) + A2_2*(v2-cos(I4_4*Pi/4))

  once (x1-3, -v1) goto L4_3 then x1, x2, v1, v2
  once (4-x1,  v1) goto L4_5 then x1, x2, v1, v2

  once (2-x2,  v2) goto L3_4 then x1, x2, v1, v2
  once (x2-1, -v2) goto L5_4 then x1, x2, v1, v2
end

at L4_5 wait v1, v2, 
             A1_1*(v1-sin(I4_5*Pi/4)) + A1_2*(v2-cos(I4_5*Pi/4)), 
             A2_1*(v1-sin(I4_5*Pi/4)) + A2_2*(v2-cos(I4_5*Pi/4))

  once (x1-4, -v1) goto L4_4 then x1, x2, v1, v2

  once (2-x2,  v2) goto L3_5 then x1, x2, v1, v2
  once (x2-1, -v2) goto L5_5 then x1, x2, v1, v2
end

at L5_1 wait v1, v2, 
             A1_1*(v1-sin(I5_1*Pi/4)) + A1_2*(v2-cos(I5_1*Pi/4)), 
             A2_1*(v1-sin(I5_1*Pi/4)) + A2_2*(v2-cos(I5_1*Pi/4))

  once (1-x1,  v1) goto L5_2 then x1, x2, v1, v2

  once (1-x2,  v2) goto L4_1 then x1, x2, v1, v2
end

at L5_2 wait v1, v2, 
             A1_1*(v1-sin(I5_2*Pi/4)) + A1_2*(v2-cos(I5_2*Pi/4)), 
             A2_1*(v1-sin(I5_2*Pi/4)) + A2_2*(v2-cos(I5_2*Pi/4))

  once (x1-1, -v1) goto L5_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L5_3 then x1, x2, v1, v2

  once (1-x2,  v2) goto L4_2 then x1, x2, v1, v2
end

at L5_3 wait v1, v2, 
             A1_1*(v1-sin(I5_3*Pi/4)) + A1_2*(v2-cos(I5_3*Pi/4)), 
             A2_1*(v1-sin(I5_3*Pi/4)) + A2_2*(v2-cos(I5_3*Pi/4))

  once (x1-2, -v1) goto L5_2 then x1, x2, v1, v2
  once (3-x1,  v1) goto L5_4 then x1, x2, v1, v2

  once (1-x2,  v2) goto L4_3 then x1, x2, v1, v2
end

at L5_4 wait v1, v2, 
             A1_1*(v1-sin(I5_4*Pi/4)) + A1_2*(v2-cos(I5_4*Pi/4)), 
             A2_1*(v1-sin(I5_4*Pi/4)) + A2_2*(v2-cos(I5_4*Pi/4))

  once (x1-3, -v1) goto L5_3 then x1, x2, v1, v2
  once (4-x1,  v1) goto L5_5 then x1, x2, v1, v2

  once (1-x2,  v2) goto L4_4 then x1, x2, v1, v2
end

at L5_5 wait v1, v2, 
             A1_1*(v1-sin(I5_5*Pi/4)) + A1_2*(v2-cos(I5_5*Pi/4)), 
             A2_1*(v1-sin(I5_5*Pi/4)) + A2_2*(v2-cos(I5_5*Pi/4))

  once (x1-4, -v1) goto L5_4 then x1, x2, v1, v2

  once (1-x2,  v2) goto L4_5 then x1, x2, v1, v2
end

prop  G[0,10](! loc L3_3) & F[0,10](loc L5_3)

param order = 20
param t_max = 10
param dump_interval = 0.1
param cm_thres = 0
