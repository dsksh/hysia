(* Automatic gearbox.
 * HHSC, Section 1.3.2, pp. 22-26. 
 * 
 * usage: ./hss.opt ../examples/gearbox.ha -n 5
 *)

let pr1 = 50
let pr2 = 32
let pr3 = 20
let pr4 = 14
let c = 0.7
let m = 1500
let g = 10
let kr1 = 3.75
let kr2 = 5.86
let kr3 = 9.375
let kr4 = 13.39
let tr = 40
let vref = 30
let omega_h = 500
let omega_l = 230
let d = R 3.1415
#let d = 1.571
let d_ = 1.571
#let d = 0
#let d_ = 0

#let v0 = 14
let v0 = R 30

var   v, (* velocity *)
      ti (* torque integrater *)

init  Q2pos, 14, 0

at Q1pos wait pr1*(kr1*(vref-v) + ti + (c/pr1)*v^2)/m - (c/m)*v^2 - g*sin(d-d_), (kr1/tr)*(vref-v)
  once (-v, true) goto Q1neg then v, ti
  once (v - (1/pr1)*omega_h, true) goto Q2pos then v, pr1*ti/pr2
end

at Q1neg wait pr1*(kr1*(vref-v) + ti - (c/pr1)*v^2)/m + (c/m)*v^2 - g*sin(d-d_), (kr1/tr)*(vref-v)
  once (v, true) goto Q2pos then v, ti
  once (v - (1/pr1)*omega_h, true) goto Q2neg then v, pr1*ti/pr2
end

at Q2pos wait pr2*(kr2*(vref-v) + ti + (c/pr2)*v^2)/m - (c/m)*v^2 - g*sin(d-d_), (kr2/tr)*(vref-v)
  once (-v, true) goto Q2neg then v, ti
  once (v - (1/pr2)*omega_h, true) goto Q3pos then v, pr2*ti/pr3
  once ((1/pr2)*omega_l - v, true) goto Q1pos then v, pr2*ti/pr1
end

at Q2neg wait pr2*(kr2*(vref-v) + ti - (c/pr2)*v^2)/m + (c/m)*v^2 - g*sin(d-d_), (kr2/tr)*(vref-v)
  once (v, true) goto Q2pos then v, ti
  once (v - (1/pr2)*omega_h, true) goto Q3neg then v, pr2*ti/pr3
  once ((1/pr2)*omega_l - v, true) goto Q1neg then v, pr2*ti/pr1
end

at Q3pos wait pr3*(kr3*(vref-v) + ti + (c/pr3)*v^2)/m - (c/m)*v^2 - g*sin(d-d_), (kr3/tr)*(vref-v)
  once (-v, true) goto Q3neg then v, ti
  once (v - (1/pr3)*omega_h, true) goto Q4pos then v, pr3*ti/pr4
  once ((1/pr3)*omega_l - v, true) goto Q2pos then v, pr3*ti/pr2
end

at Q3neg wait pr3*(kr3*(vref-v) + ti - (c/pr3)*v^2)/m + (c/m)*v^2 - g*sin(d-d_), (kr3/tr)*(vref-v)
  once (v, true) goto Q3pos then v, ti
  once (v - (1/pr3)*omega_h, true) goto Q4neg then v, pr3*ti/pr4
  once ((1/pr3)*omega_l - v, true) goto Q2neg then v, pr3*ti/pr2
end

at Q4pos wait pr4*(kr4*(vref-v) + ti + (c/pr4)*v^2)/m - (c/m)*v^2 - g*sin(d-d_), (kr4/tr)*(vref-v)
  once (-v, true) goto Q4neg then v, ti
  once ((1/pr4)*omega_l - v, true) goto Q3pos then v, pr4*ti/pr3
end

at Q4neg wait pr4*(kr4*(vref-v) + ti - (c/pr4)*v^2)/m + (c/m)*v^2 - g*sin(d-d_), (kr4/tr)*(vref-v)
  once (v, true) goto Q4pos then v, ti
  once ((1/pr4)*omega_l - v, true) goto Q3neg then v, pr4*ti/pr3
end


param order = 20
param t_max = 1000
param h_min   = 1e-14
param epsilon = 1e-14
param dump_interval = 0.1
