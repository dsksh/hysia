var   x1,x2,vx1,vx2

init  L, -0.5,0, 0,1

at L wait vx1, vx2, 0, 0
  once (
		-4*x1^8 + 5*x1^10 - 48*x1^6 * x2^2 + 25*x1^8 * x2^2 + 40*x1^4 * x2^4 + 50*x1^6 * x2^4 - 48*x1^2 * x2^6 + 50*x1^4 * x2^6 - 4*x2^8 + 25*x1^2 * x2^8 + 5*x2^10,

		vx1 * (-32*x1^7 + 50*x1^9 - 288*x1^5 * x2^2 + 200*x1^7 * x2^2 + 160*x1^3 * x2^4 + 300*x1^5 * x2^4 - 96*x1 * x2^6 + 200*x1^3 * x2^6 + 50*x1 * x2^8) + vx2*(-96*x1^6 * x2 + 50*x1^8 * x2 + 160*x1^4 * x2^3 + 200*x1^6 * x2^3 - 288*x1^2 * x2^5 + 300*x1^4 * x2^5 - 32*x2^7 + 200*x1^2 * x2^7 + 50*x2^9)

#		-vx1 * (-32*x1^7 + 50*x1^9 - 288*x1^5*x2^2 + 200*x1^7*x2^2 + 160*x1^3*x2^4 + 300*x1^5*x2^4 - 96*x1*x2^6 + 200*x1^3*x2^6 + 50*x1*x2^8) - vx2*(-96*x1^6*x2 + 50*x1^8*x2 + 160*x1^4*x2^3 + 200*x1^6*x2^3 - 288*x1^2*x2^5 + 300*x1^4*x2^5 - 32*x2^7 + 200*x1^2*x2^7 + 50*x2^9) 
#		vx1*(32*x1^7 - 50*x1^9 + 288*x1^5*x2^2 - 200*x1^7*x2^2 - 160*x1^3*x2^4 - 300*x1^5*x2^4 + 96*x1*x2^6 - 200*x1^3*x2^6 - 50*x1*x2^8) + vx2*(96*x1^6*x2 - 50*x1^8*x2 - 160*x1^4*x2^3 - 200*x1^6*x2^3 + 288*x1^2*x2^5 - 300*x1^4*x2^5 + 32*x2^7 - 200*x1^2*x2^7 - 50*x2^9)
#		-x2*vx2
	)
    goto L then 

    x1,x2,
#	(-2*vx2*x1*x2*(625*x1^16 + 200*x1^14*(-8 + 25*x2^2) + x2^12*(768 - 1600*x2^2 + 625*x2^4) + 8*x1^2*x2^10*(704 - 1000*x2^2 + 625*x2^4) + 4*x1^4*x2^8*(-3264 - 4400*x2^2 + 4375*x2^4) + 8*x1^6*x2^6*(3712 - 3000*x2^2 + 4375*x2^4) + 8*x1^10*x2^2*(704 - 2200*x2^2 + 4375*x2^4) + 4*x1^12*(192 - 2000*x2^2 + 4375*x2^4) + 2*x1^8*x2^4*(-6528 - 12000*x2^2 + 21875*x2^4)) + vx1*(-625*x1^18 + x1^16*(800 - 4375*x2^2) + x2^14*(16 - 25*x2^2)^2 + 4*x1^4*x2^10*(6464 - 6000*x2^2 + 3125*x2^4) - 4*x1^14*(64 - 2000*x2^2 + 3125*x2^4) + 2*x1^8*x2^6*(20864 + 4375*x2^4) + x1^2*x2^12*(2304 - 8000*x2^2 + 4375*x2^4) + 4*x1^6*x2^8*(-10432 - 6800*x2^2 + 4375*x2^4) - 4*x1^12*x2^2*(576 - 6000*x2^2 + 4375*x2^4) + x1^10*(-25856*x2^4 + 27200*x2^6 - 8750*x2^8))) / ((x1^2 + x2^2)^3*(625*x1^12 + x2^8*(16 - 25*x2^2)^2 + 50*x1^10*(-16 + 75*x2^2) + 2*x1^2*x2^6*(3072 - 5200*x2^2 + 1875*x2^4) + 4*x1^6*x2^2*(1536 - 400*x2^2 + 3125*x2^4) + x1^8*(256 - 10400*x2^2 + 9375*x2^4) + x1^4*x2^4*(-8704 - 1600*x2^2 + 9375*x2^4))),
#  	(-2*vx1*x1*x2*(625*x1^16 + 200*x1^14*(-8 + 25*x2^2) + x2^12*(768 - 1600*x2^2 + 625*x2^4) + 8*x1^2*x2^10*(704 - 1000*x2^2 + 625*x2^4) + 4*x1^4*x2^8*(-3264 - 4400*x2^2 + 4375*x2^4) + 8*x1^6*x2^6*(3712 - 3000*x2^2 + 4375*x2^4) + 8*x1^10*x2^2*(704 - 2200*x2^2 + 4375*x2^4) + 4*x1^12*(192 - 2000*x2^2 + 4375*x2^4) + 2*x1^8*x2^4*(-6528 - 12000*x2^2 + 21875*x2^4)) + vx2*(625*x1^18 - x2^14*(16 - 25*x2^2)^2 + 25*x1^16*(-32 + 175*x2^2) + 4*x1^6*x2^8*(10432 + 6800*x2^2 - 4375*x2^4) + x1^2*x2^12*(-2304 + 8000*x2^2 - 4375*x2^4) - 4*x1^4*x2^10*(6464 - 6000*x2^2 + 3125*x2^4) + 4*x1^14*(64 - 2000*x2^2 + 3125*x2^4) - 2*x1^8*x2^6*(20864 + 4375*x2^4) + 4*x1^12*x2^2*(576 - 6000*x2^2 + 4375*x2^4) + x1^10*(25856*x2^4 - 27200*x2^6 + 8750*x2^8)))/ ((x1^2 + x2^2)^3*(625*x1^12 + x2^8*(16 - 25*x2^2)^2 + 50*x1^10*(-16 + 75*x2^2) + 2*x1^2*x2^6*(3072 - 5200*x2^2 + 1875*x2^4) + 4*x1^6*x2^2*(1536 - 400*x2^2 + 3125*x2^4) + x1^8*(256 - 10400*x2^2 + 9375*x2^4) + x1^4*x2^4*(-8704 - 1600*x2^2 + 9375*x2^4)))
	vx1,-vx2
end

param order = 10
param t_max = 4
param h_min   = 1e-14
param epsilon = 1e-14
#param dump_interval = 1
param cm_thres = 0
