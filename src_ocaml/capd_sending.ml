open Capd_stubs
open Ptree

module SM = Map.Make(String)

let send_var env (_,id) =
  Printf.printf "send var: %s\n" id; 
  let index = put_variable id in
  SM.add id index env

let fun_un_op = function
  | Osqr  -> put_sqr_node
  | Osqrt -> put_sqrt_node
  | Oexp  -> put_exp_node
  | Olog  -> put_log_node
  | Osin  -> put_sin_node
  | Ocos  -> put_cos_node
  | Oatan -> put_atan_node
  | Oasin -> put_asin_node
  | Oacos -> put_acos_node

let fun_bin_op = function
  | Oadd -> put_sum_node
  | Osub -> put_dif_node
  | Omul -> put_mul_node
  | Odiv -> put_div_node
  | Opow -> put_pow_node

let rec send_expr env = function
  | Pvar id -> (*Printf.printf "send var: %s %d\n" id (SM.find id env); *)
      put_var_node (SM.find id env)
  | Pval v  -> (*Printf.printf "send val: %f\n" v; *)
      put_scalar_node v
  | Papp (op,(_,e)) -> 
      (*fprintf fmt "%s %a" (sprint_un_op op) print_expr e*)
      send_expr env e;
      fun_un_op op ()
  | Papp2 (op,(_,e1),(_,e2)) -> 
      (*fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op) print_expr e2*)
      send_expr env e1;
      send_expr env e2;
      fun_bin_op op ()

let send_tree env (_,expr) =
  send_expr env expr;
  put_tree ()

let send_vf env (_,(var,def)) =
  let env = List.fold_left send_var env var in
  List.map (send_tree env) def


let send_iv (_,(_t,v)) =
  List.map put_value v


let send_ptree (_,(vf,iv,ps)) =
  send_vf SM.empty vf;
  send_iv iv;
  ()
