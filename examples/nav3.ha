let Pi = 3.141592
let A1_1 = -1.2
let A1_2 = 0.1
let A2_1 = 0.1
let A2_2 = -1.2

let I1_1 = 0
let I1_2 = 2
let I1_3 = 4
let I2_1 = 4
let I2_2 = 3
let I2_3 = 4
let I3_1 = 2
let I3_2 = 2
let I3_3 = 0

var   x1, x2, v1, v2

init  L2_1, 0.5, 1.5, 0.1, 0


at L1_1 wait v1, v2, 
             A1_1*(v1-sin(I1_1*Pi/4)) + A1_2*(v2-cos(I1_1*Pi/4)), 
             A2_1*(v1-sin(I1_1*Pi/4)) + A2_2*(v2-cos(I1_1*Pi/4))

  once (1-x1,  v1) goto L1_2 then x1, x2, v1, v2

  once (x2-2, -v2) goto L2_1 then x1, x2, v1, v2
end

at L1_2 wait v1, v2, 
             A1_1*(v1-sin(I1_2*Pi/4)) + A1_2*(v2-cos(I1_2*Pi/4)), 
             A2_1*(v1-sin(I1_2*Pi/4)) + A2_2*(v2-cos(I1_2*Pi/4))

  once (x1-1, -v1) goto L1_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L1_3 then x1, x2, v1, v2

  once (x2-2,  v2) goto L2_2 then x1, x2, v1, v2
end

at L1_3 wait v1, v2, 
             A1_1*(v1-sin(I1_3*Pi/4)) + A1_2*(v2-cos(I1_3*Pi/4)), 
             A2_1*(v1-sin(I1_3*Pi/4)) + A2_2*(v2-cos(I1_3*Pi/4))

  once (x1-2, -v1) goto L1_2 then x1, x2, v1, v2
  once (x2-2, -v2) goto L2_3 then x1, x2, v1, v2
end

at L2_1 wait v1, v2, 
             A1_1*(v1-sin(I2_1*Pi/4)) + A1_2*(v2-cos(I2_1*Pi/4)), 
             A2_1*(v1-sin(I2_1*Pi/4)) + A2_2*(v2-cos(I2_1*Pi/4))

  once (1-x1,  v1) goto L2_2 then x1, x2, v1, v2
  once (2-x2,  v2) goto L1_1 then x1, x2, v1, v2
  once (x2-1, -v2) goto L3_1 then x1, x2, v1, v2
end

at L2_2 wait v1, v2, 
             A1_1*(v1-sin(I2_2*Pi/4)) + A1_2*(v2-cos(I2_2*Pi/4)), 
             A2_1*(v1-sin(I2_2*Pi/4)) + A2_2*(v2-cos(I2_2*Pi/4))

  once (x1-1, -v1) goto L2_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L2_3 then x1, x2, v1, v2
  once (2-x2,  v2) goto L1_2 then x1, x2, v1, v2
  once (x2-1, -v2) goto L3_2 then x1, x2, v1, v2
end

at L2_3 wait v1, v2, 
             A1_1*(v1-sin(I2_3*Pi/4)) + A1_2*(v2-cos(I2_3*Pi/4)), 
             A2_1*(v1-sin(I2_3*Pi/4)) + A2_2*(v2-cos(I2_3*Pi/4))

  once (x1-2, -v1) goto L2_2 then x1, x2, v1, v2
  once (2-x2,  v2) goto L1_3 then x1, x2, v1, v2
  once (x2-1, -v2) goto L3_3 then x1, x2, v1, v2
end

at L3_1 wait v1, v2, 
             A1_1*(v1-sin(I3_1*Pi/4)) + A1_2*(v2-cos(I3_1*Pi/4)), 
             A2_1*(v1-sin(I3_1*Pi/4)) + A2_2*(v2-cos(I3_1*Pi/4))

  once (1-x1,  v1) goto L3_2 then x1, x2, v1, v2
  once (1-x2,  v2) goto L2_1 then x1, x2, v1, v2
end

at L3_2 wait v1, v2, 
             A1_1*(v1-sin(I3_2*Pi/4)) + A1_2*(v2-cos(I3_2*Pi/4)), 
             A2_1*(v1-sin(I3_2*Pi/4)) + A2_2*(v2-cos(I3_2*Pi/4))

  once (x1-1, -v1) goto L3_1 then x1, x2, v1, v2
  once (2-x1,  v1) goto L3_3 then x1, x2, v1, v2
  once (1-x2,  v2) goto L2_2 then x1, x2, v1, v2
end

at L3_3 wait v1, v2, 
             A1_1*(v1-sin(I3_3*Pi/4)) + A1_2*(v2-cos(I3_3*Pi/4)), 
             A2_1*(v1-sin(I3_3*Pi/4)) + A2_2*(v2-cos(I3_3*Pi/4))

  once (x1-2, -v1) goto L3_2 then x1, x2, v1, v2
  once (1-x2,  v2) goto L2_3 then x1, x2, v1, v2
end


param order = 20
param t_max = 10
param dump_interval = 0.01
