let Pi = 3.141592
let A1_1 = -0.8
let A1_2 = -0.2
let A2_1 = -0.1
let A2_2 = -0.8

let I1_1 = 2
let I1_2 = 4
let I1_3 = 6
let I1_4 = 6
let I1_5 = 6
let I2_1 = 2
let I2_2 = 4
let I2_3 = 7
let I2_4 = 7
let I2_5 = 4
let I3_1 = 2
let I3_2 = 4
let I3_3 = 0
let I3_4 = 3
let I3_5 = 4
let I4_1 = 2
let I4_2 = 4
let I4_3 = 6
let I4_4 = 6
let I4_5 = 6
let I5_1 = 2
let I5_2 = 0
let I5_3 = 0
let I5_4 = 0
let I5_5 = 0

let vx0 = R 1
let vy0 = R 1

var   x1, x2, v1, v2

init  L2_4, 3.5, 3.5, vx0, vy0
final L1_1_, L1_2_, L1_3_, L1_4_, L1_5_, L2_1_, L2_2_, L2_3_, L2_4_, L2_5_, L3_1_, L3_2_, L3_3_, L3_4_, L3_5_, L4_1_, L4_2_, L4_3_, L4_4_, L4_5_, L5_1_, L5_2_, L5_3_, L5_4_, L5_5_


at L1_1 wait v1, v2, 
             A1_1*(v1-sin(I1_1*Pi/4)) + A1_2*(v2-cos(I1_1*Pi/4)), 
             A2_1*(v1-sin(I1_1*Pi/4)) + A2_2*(v2-cos(I1_1*Pi/4))
  invariant true, 1-x1, x2-4

  watch (1-x1, -v1) goto L1_2 then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_1 then x1, x2, v1, v2
end

at L1_2 wait v1, v2, 
             A1_1*(v1-sin(I1_2*Pi/4)) + A1_2*(v2-cos(I1_2*Pi/4)), 
             A2_1*(v1-sin(I1_2*Pi/4)) + A2_2*(v2-cos(I1_2*Pi/4))
  invariant true, x1-1, 2-x1, x2-4
  watch (x1-1,  v1) goto L1_1 then x1, x2, v1, v2
  watch (2-x1, -v1) goto L1_3 then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_2 then x1, x2, v1, v2
end

at L1_3 wait v1, v2, 
             A1_1*(v1-sin(I1_3*Pi/4)) + A1_2*(v2-cos(I1_3*Pi/4)), 
             A2_1*(v1-sin(I1_3*Pi/4)) + A2_2*(v2-cos(I1_3*Pi/4))
  invariant true, x1-2, 3-x1, x2-4
  watch (x1-2,  v1) goto L1_2 then x1, x2, v1, v2
  watch (3-x1, -v1) goto L1_4 then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_3 then x1, x2, v1, v2
end

at L1_4 wait v1, v2, 
             A1_1*(v1-sin(I1_4*Pi/4)) + A1_2*(v2-cos(I1_4*Pi/4)), 
             A2_1*(v1-sin(I1_4*Pi/4)) + A2_2*(v2-cos(I1_4*Pi/4))
  invariant true, x1-3, 4-x1, x2-4
  watch (x1-3,  v1) goto L1_3 then x1, x2, v1, v2
  watch (4-x1, -v1) goto L1_5 then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_4 then x1, x2, v1, v2
end

at L1_5 wait v1, v2, 
             A1_1*(v1-sin(I1_5*Pi/4)) + A1_2*(v2-cos(I1_5*Pi/4)), 
             A2_1*(v1-sin(I1_5*Pi/4)) + A2_2*(v2-cos(I1_5*Pi/4))
  invariant true, x1-4, x2-4
  watch (x1-4,  v1) goto L1_4 then x1, x2, v1, v2


  watch (x2-4,  v2) goto L2_5 then x1, x2, v1, v2
end

at L2_1 wait v1, v2, 
             A1_1*(v1-sin(I2_1*Pi/4)) + A1_2*(v2-cos(I2_1*Pi/4)), 
             A2_1*(v1-sin(I2_1*Pi/4)) + A2_2*(v2-cos(I2_1*Pi/4))
  invariant true, 1-x1, 4-x2, x2-3

  watch (1-x1, -v1) goto L2_2 then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_1 then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_1 then x1, x2, v1, v2
end

at L2_2 wait v1, v2, 
             A1_1*(v1-sin(I2_2*Pi/4)) + A1_2*(v2-cos(I2_2*Pi/4)), 
             A2_1*(v1-sin(I2_2*Pi/4)) + A2_2*(v2-cos(I2_2*Pi/4))
  invariant true, x1-1, 2-x1, 4-x2, x2-3
  watch (x1-1,  v1) goto L2_1 then x1, x2, v1, v2
  watch (2-x1, -v1) goto L2_3 then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_2 then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_2 then x1, x2, v1, v2
end

at L2_3 wait v1, v2, 
             A1_1*(v1-sin(I2_3*Pi/4)) + A1_2*(v2-cos(I2_3*Pi/4)), 
             A2_1*(v1-sin(I2_3*Pi/4)) + A2_2*(v2-cos(I2_3*Pi/4))
  invariant true, x1-2, 3-x1, 4-x2, x2-3
  watch (x1-2,  v1) goto L2_2 then x1, x2, v1, v2
  watch (3-x1, -v1) goto L2_4 then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_3 then x1, x2, v1, v2
#  watch (x2-3,  v2) goto L3_3 then x1, x2, v1, v2
end

at L2_4 wait v1, v2, 
             A1_1*(v1-sin(I2_4*Pi/4)) + A1_2*(v2-cos(I2_4*Pi/4)), 
             A2_1*(v1-sin(I2_4*Pi/4)) + A2_2*(v2-cos(I2_4*Pi/4))
  invariant true, x1-3, 4-x1, 4-x2, x2-3
  watch (x1-3,  v1) goto L2_3 then x1, x2, v1, v2
  watch (4-x1, -v1) goto L2_5 then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_4 then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_4 then x1, x2, v1, v2
end

at L2_5 wait v1, v2, 
             A1_1*(v1-sin(I2_5*Pi/4)) + A1_2*(v2-cos(I2_5*Pi/4)), 
             A2_1*(v1-sin(I2_5*Pi/4)) + A2_2*(v2-cos(I2_5*Pi/4))
  invariant true, x1-4, 4-x2, x2-3
  watch (x1-4,  v1) goto L2_4 then x1, x2, v1, v2

  watch (4-x2, -v2) goto L1_5 then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_5 then x1, x2, v1, v2
end

at L3_1 wait v1, v2, 
             A1_1*(v1-sin(I3_1*Pi/4)) + A1_2*(v2-cos(I3_1*Pi/4)), 
             A2_1*(v1-sin(I3_1*Pi/4)) + A2_2*(v2-cos(I3_1*Pi/4))
  invariant true, 1-x1, 3-x2, x2-2

  watch (1-x1, -v1) goto L3_2 then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_1 then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_1 then x1, x2, v1, v2
end

at L3_2 wait v1, v2, 
             A1_1*(v1-sin(I3_2*Pi/4)) + A1_2*(v2-cos(I3_2*Pi/4)), 
             A2_1*(v1-sin(I3_2*Pi/4)) + A2_2*(v2-cos(I3_2*Pi/4))
  invariant true, x1-1, 2-x1, 3-x2, x2-2
  watch (x1-1,  v1) goto L3_1 then x1, x2, v1, v2
#  watch (2-x1, -v1) goto L3_3 then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_2 then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_2 then x1, x2, v1, v2
end

(*at L3_3 wait v1, v2, 
             A1_1*(v1-sin(I3_3*Pi/4)) + A1_2*(v2-cos(I3_3*Pi/4)), 
             A2_1*(v1-sin(I3_3*Pi/4)) + A2_2*(v2-cos(I3_3*Pi/4))
  invariant true, x1-2, 3-x1, 3-x2, x2-2
  watch (x1-2,  v1) goto L3_2 then x1, x2, v1, v2
  watch (3-x1, -v1) goto L3_4 then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_3 then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_3 then x1, x2, v1, v2
end
*)

at L3_4 wait v1, v2, 
             A1_1*(v1-sin(I3_4*Pi/4)) + A1_2*(v2-cos(I3_4*Pi/4)), 
             A2_1*(v1-sin(I3_4*Pi/4)) + A2_2*(v2-cos(I3_4*Pi/4))
  invariant true, x1-3, 4-x1, 3-x2, x2-2
#  watch (x1-3,  v1) goto L3_3 then x1, x2, v1, v2
  watch (4-x1, -v1) goto L3_5 then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_4 then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_4 then x1, x2, v1, v2
end

at L3_5 wait v1, v2, 
             A1_1*(v1-sin(I3_5*Pi/4)) + A1_2*(v2-cos(I3_5*Pi/4)), 
             A2_1*(v1-sin(I3_5*Pi/4)) + A2_2*(v2-cos(I3_5*Pi/4))
  invariant true, x1-4, 3-x2, x2-2
  watch (x1-4,  v1) goto L3_4 then x1, x2, v1, v2

  watch (3-x2, -v2) goto L2_5 then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_5 then x1, x2, v1, v2
end

at L4_1 wait v1, v2, 
             A1_1*(v1-sin(I4_1*Pi/4)) + A1_2*(v2-cos(I4_1*Pi/4)), 
             A2_1*(v1-sin(I4_1*Pi/4)) + A2_2*(v2-cos(I4_1*Pi/4))
  invariant true, 1-x1, 2-x2, x2-1

  watch (1-x1, -v1) goto L4_2 then x1, x2, v1, v2
  watch (2-x2, -v2) goto L3_1 then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_1 then x1, x2, v1, v2
end

at L4_2 wait v1, v2, 
             A1_1*(v1-sin(I4_2*Pi/4)) + A1_2*(v2-cos(I4_2*Pi/4)), 
             A2_1*(v1-sin(I4_2*Pi/4)) + A2_2*(v2-cos(I4_2*Pi/4))
  invariant true, x1-1, 2-x1, 2-x2, x2-1
  watch (x1-1,  v1) goto L4_1 then x1, x2, v1, v2
  watch (2-x1, -v1) goto L4_3 then x1, x2, v1, v2
  watch (2-x2, -v2) goto L3_2 then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_2_ then x1, x2, v1, v2
end

at L4_3 wait v1, v2, 
             A1_1*(v1-sin(I4_3*Pi/4)) + A1_2*(v2-cos(I4_3*Pi/4)), 
             A2_1*(v1-sin(I4_3*Pi/4)) + A2_2*(v2-cos(I4_3*Pi/4))
  invariant true, x1-2, 3-x1, 2-x2, x2-1
  watch (x1-2,  v1) goto L4_2 then x1, x2, v1, v2
  watch (3-x1, -v1) goto L4_4 then x1, x2, v1, v2
#  watch (2-x2, -v2) goto L3_3 then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_3 then x1, x2, v1, v2
end

at L4_4 wait v1, v2, 
             A1_1*(v1-sin(I4_4*Pi/4)) + A1_2*(v2-cos(I4_4*Pi/4)), 
             A2_1*(v1-sin(I4_4*Pi/4)) + A2_2*(v2-cos(I4_4*Pi/4))
  invariant true, x1-3, 4-x1, 2-x2, x2-1
  watch (x1-3,  v1) goto L4_3 then x1, x2, v1, v2
  watch (4-x1, -v1) goto L4_5 then x1, x2, v1, v2
  watch (2-x2, -v2) goto L3_4 then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_4 then x1, x2, v1, v2
end

at L4_5 wait v1, v2, 
             A1_1*(v1-sin(I4_5*Pi/4)) + A1_2*(v2-cos(I4_5*Pi/4)), 
             A2_1*(v1-sin(I4_5*Pi/4)) + A2_2*(v2-cos(I4_5*Pi/4))
  invariant true, x1-4, 2-x2, x2-1
  watch (x1-4,  v1) goto L4_4 then x1, x2, v1, v2

  watch (2-x2, -v2) goto L3_5 then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_5 then x1, x2, v1, v2
end

at L5_1 wait v1, v2, 
             A1_1*(v1-sin(I5_1*Pi/4)) + A1_2*(v2-cos(I5_1*Pi/4)), 
             A2_1*(v1-sin(I5_1*Pi/4)) + A2_2*(v2-cos(I5_1*Pi/4))
  invariant true, 1-x1, 1-x2

  watch (1-x1, -v1) goto L5_2_ then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_1 then x1, x2, v1, v2

end

(*at L5_2 wait v1, v2, 
             A1_1*(v1-sin(I5_2*Pi/4)) + A1_2*(v2-cos(I5_2*Pi/4)), 
             A2_1*(v1-sin(I5_2*Pi/4)) + A2_2*(v2-cos(I5_2*Pi/4))
  invariant true, x1-1, 2-x1, 1-x2
  watch (x1-1,  v1) goto L5_1 then x1, x2, v1, v2
  watch (2-x1, -v1) goto L5_3 then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_2 then x1, x2, v1, v2

end
*)

at L5_3 wait v1, v2, 
             A1_1*(v1-sin(I5_3*Pi/4)) + A1_2*(v2-cos(I5_3*Pi/4)), 
             A2_1*(v1-sin(I5_3*Pi/4)) + A2_2*(v2-cos(I5_3*Pi/4))
  invariant true, x1-2, 3-x1, 1-x2
  watch (x1-2,  v1) goto L5_2_ then x1, x2, v1, v2
  watch (3-x1, -v1) goto L5_4 then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_3 then x1, x2, v1, v2

end

at L5_4 wait v1, v2, 
             A1_1*(v1-sin(I5_4*Pi/4)) + A1_2*(v2-cos(I5_4*Pi/4)), 
             A2_1*(v1-sin(I5_4*Pi/4)) + A2_2*(v2-cos(I5_4*Pi/4))
  invariant true, x1-3, 4-x1, 1-x2
  watch (x1-3,  v1) goto L5_3 then x1, x2, v1, v2
  watch (4-x1, -v1) goto L5_5 then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_4 then x1, x2, v1, v2

end

at L5_5 wait v1, v2, 
             A1_1*(v1-sin(I5_5*Pi/4)) + A1_2*(v2-cos(I5_5*Pi/4)), 
             A2_1*(v1-sin(I5_5*Pi/4)) + A2_2*(v2-cos(I5_5*Pi/4))
  invariant true, x1-4, 1-x2
  watch (x1-4,  v1) goto L5_4 then x1, x2, v1, v2

  watch (1-x2, -v2) goto L4_5 then x1, x2, v1, v2

end


(* final states. *)

at L1_1_ wait v1, v2, 
             A1_1*(v1-sin(I1_1*Pi/4)) + A1_2*(v2-cos(I1_1*Pi/4)), 
             A2_1*(v1-sin(I1_1*Pi/4)) + A2_2*(v2-cos(I1_1*Pi/4))
  invariant true, 1-x1, x2-4

  watch (1-x1, -v1) goto L1_2_ then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_1_ then x1, x2, v1, v2
end

at L1_2_ wait v1, v2, 
             A1_1*(v1-sin(I1_2*Pi/4)) + A1_2*(v2-cos(I1_2*Pi/4)), 
             A2_1*(v1-sin(I1_2*Pi/4)) + A2_2*(v2-cos(I1_2*Pi/4))
  invariant true, x1-1, 2-x1, x2-4
  watch (x1-1,  v1) goto L1_1_ then x1, x2, v1, v2
  watch (2-x1, -v1) goto L1_3_ then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_2_ then x1, x2, v1, v2
end

at L1_3_ wait v1, v2, 
             A1_1*(v1-sin(I1_3*Pi/4)) + A1_2*(v2-cos(I1_3*Pi/4)), 
             A2_1*(v1-sin(I1_3*Pi/4)) + A2_2*(v2-cos(I1_3*Pi/4))
  invariant true, x1-2, 3-x1, x2-4
  watch (x1-2,  v1) goto L1_2_ then x1, x2, v1, v2
  watch (3-x1, -v1) goto L1_4_ then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_3_ then x1, x2, v1, v2
end

at L1_4_ wait v1, v2, 
             A1_1*(v1-sin(I1_4*Pi/4)) + A1_2*(v2-cos(I1_4*Pi/4)), 
             A2_1*(v1-sin(I1_4*Pi/4)) + A2_2*(v2-cos(I1_4*Pi/4))
  invariant true, x1-3, 4-x1, x2-4
  watch (x1-3,  v1) goto L1_3_ then x1, x2, v1, v2
  watch (4-x1, -v1) goto L1_5_ then x1, x2, v1, v2

  watch (x2-4,  v2) goto L2_4_ then x1, x2, v1, v2
end

at L1_5_ wait v1, v2, 
             A1_1*(v1-sin(I1_5*Pi/4)) + A1_2*(v2-cos(I1_5*Pi/4)), 
             A2_1*(v1-sin(I1_5*Pi/4)) + A2_2*(v2-cos(I1_5*Pi/4))
  invariant true, x1-4, x2-4
  watch (x1-4,  v1) goto L1_4_ then x1, x2, v1, v2


  watch (x2-4,  v2) goto L2_5_ then x1, x2, v1, v2
end

at L2_1_ wait v1, v2, 
             A1_1*(v1-sin(I2_1*Pi/4)) + A1_2*(v2-cos(I2_1*Pi/4)), 
             A2_1*(v1-sin(I2_1*Pi/4)) + A2_2*(v2-cos(I2_1*Pi/4))
  invariant true, 1-x1, 4-x2, x2-3

  watch (1-x1, -v1) goto L2_2_ then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_1_ then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_1_ then x1, x2, v1, v2
end

at L2_2_ wait v1, v2, 
             A1_1*(v1-sin(I2_2*Pi/4)) + A1_2*(v2-cos(I2_2*Pi/4)), 
             A2_1*(v1-sin(I2_2*Pi/4)) + A2_2*(v2-cos(I2_2*Pi/4))
  invariant true, x1-1, 2-x1, 4-x2, x2-3
  watch (x1-1,  v1) goto L2_1_ then x1, x2, v1, v2
  watch (2-x1, -v1) goto L2_3_ then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_2_ then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_2_ then x1, x2, v1, v2
end

at L2_3_ wait v1, v2, 
             A1_1*(v1-sin(I2_3*Pi/4)) + A1_2*(v2-cos(I2_3*Pi/4)), 
             A2_1*(v1-sin(I2_3*Pi/4)) + A2_2*(v2-cos(I2_3*Pi/4))
  invariant true, x1-2, 3-x1, 4-x2, x2-3
  watch (x1-2,  v1) goto L2_2_ then x1, x2, v1, v2
  watch (3-x1, -v1) goto L2_4_ then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_3_ then x1, x2, v1, v2
#  watch (x2-3,  v2) goto L3_3_ then x1, x2, v1, v2
end

at L2_4_ wait v1, v2, 
             A1_1*(v1-sin(I2_4*Pi/4)) + A1_2*(v2-cos(I2_4*Pi/4)), 
             A2_1*(v1-sin(I2_4*Pi/4)) + A2_2*(v2-cos(I2_4*Pi/4))
  invariant true, x1-3, 4-x1, 4-x2, x2-3
  watch (x1-3,  v1) goto L2_3_ then x1, x2, v1, v2
  watch (4-x1, -v1) goto L2_5_ then x1, x2, v1, v2
  watch (4-x2, -v2) goto L1_4_ then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_4_ then x1, x2, v1, v2
end

at L2_5_ wait v1, v2, 
             A1_1*(v1-sin(I2_5*Pi/4)) + A1_2*(v2-cos(I2_5*Pi/4)), 
             A2_1*(v1-sin(I2_5*Pi/4)) + A2_2*(v2-cos(I2_5*Pi/4))
  invariant true, x1-4, 4-x2, x2-3
  watch (x1-4,  v1) goto L2_4_ then x1, x2, v1, v2

  watch (4-x2, -v2) goto L1_5_ then x1, x2, v1, v2
  watch (x2-3,  v2) goto L3_5_ then x1, x2, v1, v2
end

at L3_1_ wait v1, v2, 
             A1_1*(v1-sin(I3_1*Pi/4)) + A1_2*(v2-cos(I3_1*Pi/4)), 
             A2_1*(v1-sin(I3_1*Pi/4)) + A2_2*(v2-cos(I3_1*Pi/4))
  invariant true, 1-x1, 3-x2, x2-2

  watch (1-x1, -v1) goto L3_2_ then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_1_ then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_1_ then x1, x2, v1, v2
end

at L3_2_ wait v1, v2, 
             A1_1*(v1-sin(I3_2*Pi/4)) + A1_2*(v2-cos(I3_2*Pi/4)), 
             A2_1*(v1-sin(I3_2*Pi/4)) + A2_2*(v2-cos(I3_2*Pi/4))
  invariant true, x1-1, 2-x1, 3-x2, x2-2
  watch (x1-1,  v1) goto L3_1_ then x1, x2, v1, v2
#  watch (2-x1, -v1) goto L3_3_ then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_2_ then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_2_ then x1, x2, v1, v2
end

(*at L3_3_ wait v1, v2, 
             A1_1*(v1-sin(I3_3*Pi/4)) + A1_2*(v2-cos(I3_3*Pi/4)), 
             A2_1*(v1-sin(I3_3*Pi/4)) + A2_2*(v2-cos(I3_3*Pi/4))
  invariant true, x1-2, 3-x1, 3-x2, x2-2
  watch (x1-2,  v1) goto L3_2_ then x1, x2, v1, v2
  watch (3-x1, -v1) goto L3_4_ then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_3_ then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_3_ then x1, x2, v1, v2
end
*)

at L3_4_ wait v1, v2, 
             A1_1*(v1-sin(I3_4*Pi/4)) + A1_2*(v2-cos(I3_4*Pi/4)), 
             A2_1*(v1-sin(I3_4*Pi/4)) + A2_2*(v2-cos(I3_4*Pi/4))
  invariant true, x1-3, 4-x1, 3-x2, x2-2
#  watch (x1-3,  v1) goto L3_3_ then x1, x2, v1, v2
  watch (4-x1, -v1) goto L3_5_ then x1, x2, v1, v2
  watch (3-x2, -v2) goto L2_4_ then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_4_ then x1, x2, v1, v2
end

at L3_5_ wait v1, v2, 
             A1_1*(v1-sin(I3_5*Pi/4)) + A1_2*(v2-cos(I3_5*Pi/4)), 
             A2_1*(v1-sin(I3_5*Pi/4)) + A2_2*(v2-cos(I3_5*Pi/4))
  invariant true, x1-4, 3-x2, x2-2
  watch (x1-4,  v1) goto L3_4_ then x1, x2, v1, v2

  watch (3-x2, -v2) goto L2_5_ then x1, x2, v1, v2
  watch (x2-2,  v2) goto L4_5_ then x1, x2, v1, v2
end

at L4_1_ wait v1, v2, 
             A1_1*(v1-sin(I4_1*Pi/4)) + A1_2*(v2-cos(I4_1*Pi/4)), 
             A2_1*(v1-sin(I4_1*Pi/4)) + A2_2*(v2-cos(I4_1*Pi/4))
  invariant true, 1-x1, 2-x2, x2-1

  watch (1-x1, -v1) goto L4_2_ then x1, x2, v1, v2
  watch (2-x2, -v2) goto L3_1_ then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_1_ then x1, x2, v1, v2
end

at L4_2_ wait v1, v2, 
             A1_1*(v1-sin(I4_2*Pi/4)) + A1_2*(v2-cos(I4_2*Pi/4)), 
             A2_1*(v1-sin(I4_2*Pi/4)) + A2_2*(v2-cos(I4_2*Pi/4))
  invariant true, x1-1, 2-x1, 2-x2, x2-1
  watch (x1-1,  v1) goto L4_1_ then x1, x2, v1, v2
  watch (2-x1, -v1) goto L4_3_ then x1, x2, v1, v2
  watch (2-x2, -v2) goto L3_2_ then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_2_ then x1, x2, v1, v2
end

at L4_3_ wait v1, v2, 
             A1_1*(v1-sin(I4_3*Pi/4)) + A1_2*(v2-cos(I4_3*Pi/4)), 
             A2_1*(v1-sin(I4_3*Pi/4)) + A2_2*(v2-cos(I4_3*Pi/4))
  invariant true, x1-2, 3-x1, 2-x2, x2-1
  watch (x1-2,  v1) goto L4_2_ then x1, x2, v1, v2
  watch (3-x1, -v1) goto L4_4_ then x1, x2, v1, v2
#  watch (2-x2, -v2) goto L3_3_ then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_3_ then x1, x2, v1, v2
end

at L4_4_ wait v1, v2, 
             A1_1*(v1-sin(I4_4*Pi/4)) + A1_2*(v2-cos(I4_4*Pi/4)), 
             A2_1*(v1-sin(I4_4*Pi/4)) + A2_2*(v2-cos(I4_4*Pi/4))
  invariant true, x1-3, 4-x1, 2-x2, x2-1
  watch (x1-3,  v1) goto L4_3_ then x1, x2, v1, v2
  watch (4-x1, -v1) goto L4_5_ then x1, x2, v1, v2
  watch (2-x2, -v2) goto L3_4_ then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_4_ then x1, x2, v1, v2
end

at L4_5_ wait v1, v2, 
             A1_1*(v1-sin(I4_5*Pi/4)) + A1_2*(v2-cos(I4_5*Pi/4)), 
             A2_1*(v1-sin(I4_5*Pi/4)) + A2_2*(v2-cos(I4_5*Pi/4))
  invariant true, x1-4, 2-x2, x2-1
  watch (x1-4,  v1) goto L4_4_ then x1, x2, v1, v2

  watch (2-x2, -v2) goto L3_5_ then x1, x2, v1, v2
  watch (x2-1,  v2) goto L5_5_ then x1, x2, v1, v2
end

at L5_1_ wait v1, v2, 
             A1_1*(v1-sin(I5_1*Pi/4)) + A1_2*(v2-cos(I5_1*Pi/4)), 
             A2_1*(v1-sin(I5_1*Pi/4)) + A2_2*(v2-cos(I5_1*Pi/4))
  invariant true, 1-x1, 1-x2

  watch (1-x1, -v1) goto L5_2_ then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_1_ then x1, x2, v1, v2

end

at L5_2_ wait v1, v2, 
             A1_1*(v1-sin(I5_2*Pi/4)) + A1_2*(v2-cos(I5_2*Pi/4)), 
             A2_1*(v1-sin(I5_2*Pi/4)) + A2_2*(v2-cos(I5_2*Pi/4))
  invariant true, x1-1, 2-x1, 1-x2
  watch (x1-1,  v1) goto L5_1_ then x1, x2, v1, v2
  watch (2-x1, -v1) goto L5_3_ then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_2_ then x1, x2, v1, v2

end

at L5_3_ wait v1, v2, 
             A1_1*(v1-sin(I5_3*Pi/4)) + A1_2*(v2-cos(I5_3*Pi/4)), 
             A2_1*(v1-sin(I5_3*Pi/4)) + A2_2*(v2-cos(I5_3*Pi/4))
  invariant true, x1-2, 3-x1, 1-x2
  watch (x1-2,  v1) goto L5_2_ then x1, x2, v1, v2
  watch (3-x1, -v1) goto L5_4_ then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_3_ then x1, x2, v1, v2

end

at L5_4_ wait v1, v2, 
             A1_1*(v1-sin(I5_4*Pi/4)) + A1_2*(v2-cos(I5_4*Pi/4)), 
             A2_1*(v1-sin(I5_4*Pi/4)) + A2_2*(v2-cos(I5_4*Pi/4))
  invariant true, x1-3, 4-x1, 1-x2
  watch (x1-3,  v1) goto L5_3_ then x1, x2, v1, v2
  watch (4-x1, -v1) goto L5_5_ then x1, x2, v1, v2
  watch (1-x2, -v2) goto L4_4_ then x1, x2, v1, v2

end

at L5_5_ wait v1, v2, 
             A1_1*(v1-sin(I5_5*Pi/4)) + A1_2*(v2-cos(I5_5*Pi/4)), 
             A2_1*(v1-sin(I5_5*Pi/4)) + A2_2*(v2-cos(I5_5*Pi/4))
  invariant true, x1-4, 1-x2
  watch (x1-4,  v1) goto L5_4_ then x1, x2, v1, v2

  watch (1-x2, -v2) goto L4_5_ then x1, x2, v1, v2

end


param order = 20
param t_max = 10
param dump_interval = 0.1

