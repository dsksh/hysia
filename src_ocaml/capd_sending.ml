open Capd_sending_stubs
open Hashcons
open Model_common
open Model
open Util

module SM = Map.Make(String)

let send_var env id =
  let index = put_variable id in
  SM.add id index env

(*let send_param env (id,v) =
  let index = match v with
  | Point v -> set_param id v v
  | Interval (l,u) -> set_param id l u
  in
  SM.add id index env
*)
let send_param env (id,_) = 
  let index = put_param id in
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

let rec send_expr env e = match e.node with
  | Var id -> (*Printf.printf "send var: %s %d\n" id (SM.find id env); *)
      if SM.mem id env then put_var_node (SM.find id env)
      else error (UnknownId id)
  (*| Val (Point v) -> (*Printf.printf "send val: %f\n" v;*)
      put_scalar_node v v*)
  | Val v ->
      put_scalar_node v.inf v.sup
  | App (op,e) -> 
      (*fprintf fmt "%s %a" (sprint_un_op op) print_expr e*)
      send_expr env e;
      fun_un_op op ()
  | App2 (op,e1,e2) -> 
      (*fprintf fmt "(%a %s %a)" print_expr e1 (sprint_bin_op op) print_expr e2*)
      send_expr env e1;
      send_expr env e2;
      fun_bin_op op ()
  (*| Int _ -> assert false*)
  | _ ->
      (* TODO *)
      Printf.printf "unknown expr!"

let send_dual put_tree put_dtree env i dual =
  let (e,d) = dual.node in
  send_expr env e;
  put_tree i;

  let send_dtree j d =
    send_expr env d;
    put_dtree i j
  in
  mapi send_dtree d

(*let send_der lid env i dual =
  let (e,d) = dual.node in
  send_expr env e;
  put_der_tree lid i;

  let send_dtree j d =
    send_expr env d;
    put_der_dtree lid i j
  in
  mapi send_dtree d

let send_inv lid env i (inv,norm) =
  let (e,d) = inv.node in
  send_expr env e;
  put_inv_tree lid i;

  let send_dtree j d =
    send_expr env d;
    put_inv_dtree lid i j
  in
  mapi send_dtree d
*)

let send_der lid env i dual =
  send_dual (put_der_tree lid) (put_der_dtree lid) env i dual

let send_inv lid env i (inv,norm) =
  send_dual (put_inv_tree lid) (put_inv_dtree lid) env i inv;
  send_dual (put_inv_normal_tree lid) (put_inv_normal_dtree lid) env i norm 

let send_ap lid env i ap =
  send_dual (put_ap_tree lid) (put_ap_dtree lid) env i ap

let send_ap_norm lid env i dual =
  send_dual (put_ap_normal_tree lid) (put_ap_normal_dtree lid) env i dual

let send_grd lid eid s env dual =
  let (e,d) = dual.node in
  send_expr env e;
  put_grd_tree lid eid s;

  let send_dtree j d =
    send_expr env d;
    put_grd_dtree lid eid s j
  in
  mapi send_dtree d

let send_grd_h lid eid env dual = send_grd lid eid 0 env dual
let send_grd_g lid eid env i dual = 
    (* FIXME *)
    (*if i = 0 then send_grd lid eid 1 env dual else []*)
    send_grd lid eid (i+1) env dual

let send_jump lid eid env i dual =
  let (e,d) = dual.node in
  send_expr env e;
  put_jump_tree lid eid i;

  let send_dtree j d =
    send_expr env d;
    put_jump_dtree lid eid i j
  in
  mapi send_dtree d


let send_init env v =
  (*let send = function
    | Point v -> put_value v v
    | Interval (l,u) -> put_value l u
  in*)
  let send e =
    send_expr env e;
    put_value ()
  in
  List.map send v

let send_edge lid env eid (_,grd_h,grd_g,dst,jump) =
  put_edge lid dst;
  send_grd_h lid eid env grd_h;
  mapi (send_grd_g lid eid env) grd_g;
  mapi (send_jump lid eid env) jump

let send_loc env (id,der,inv,edges,aps,ap_norms) =
  put_location id;
  mapi (send_der id env) der;
  mapi (send_inv id env) inv;
  mapi (send_edge id env) edges;
  mapi (send_ap id env) aps;
  mapi (send_ap_norm id env) ap_norms;
  ()

let send_model (ps,vars,(iloc,iexpr),locs) aps =
  initialize (List.length vars) (List.length ps);
  let env = SM.empty in
  let env = List.fold_left send_var env vars in
  let env = List.fold_left send_param env ps in
  List.map (send_loc env) locs;

  let set_param (id,bnd) = set_param id (Random.float bnd) in
  List.map set_param ps;
  send_init env iexpr;

  (*mapi (send_ap env) aps;*)
  ()



let send_solving_params params =
  Model_common.MParam.iter set_solving_param params;
  ()
