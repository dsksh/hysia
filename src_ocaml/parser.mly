%{
  open Model_common
  open Ptree

  let env = ref MParam.empty
  let set_param id v = env := MParam.add id v !env

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr nd = loc (), nd
  let mk_ratio n d = loc (), (n,d)

  let mk_var id = loc (), id
  let mk_var_l v = loc (), v
  let mk_param id v = loc (), (id, v)
  let mk_def_l def = loc (), def
  let mk_init v = loc (), v

  let mk_ptree nd = nd, !env
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token EQ
%token LP
%token RP
%token LB
%token RB
%token COM
%token SCOL

%token MIN
%token PLUS
%token MUL
%token DIV
%token POW

%token SQRT
%token EXP
%token LOG
%token SIN
%token COS
%token ATAN
%token ASIN
%token ACOS

%token VAR
%token DER
%token INIT
%token GRD
%token JUMP
%token PARAM

%token FUN
%token VAL

%token EOF

%start main
%type <Ptree.t * float Model_common.MParam.t> main

%%

main :
  | statements solver_params { mk_ptree $1 }
; 

/**/

statements :
  | VAR var_vec SCOL statements
    { let _,der,init,grd,jmp,param = $4 in 
	(mk_var_l $2),der,init,grd,jmp,param }
  | DER expr_vec SCOL statements
    { let var,_,init,grd,jmp,param = $4 in 
	var,(mk_def_l $2),init,grd,jmp,param }
  | INIT /*interval_vec*/ expr_vec SCOL statements
    { let var,der,_,grd,jmp,param = $4 in 
	var,der,(mk_init $2),grd,jmp,param }
  | GRD expr SCOL statements
    { let var,der,init,_,jmp,param = $4 in 
	var,der,init,$2,jmp,param }
  | JUMP expr_vec SCOL statements
    { let var,der,init,grd,jmp,param = $4 in 
	var,der,init,grd,(mk_def_l $2),param }
  | PARAM ID EQ interval SCOL statements
    { let var,der,init,grd,jmp,param = $6 in 
	var,der,init,grd,jmp,((mk_param $2 $4)::param) }

  | FUN var_vec_old EQ expr_vec SCOL statements
    { let var,_,init,grd,jmp,param = $6 in 
	(mk_var_l $2),(mk_def_l $4),init,grd,jmp,param }
  | VAL integer EQ /*interval_vec*/ expr_vec SCOL statements
    { let var,der,_,grd,jmp,param  = $6 in 
	var,der,(mk_init $4),grd,jmp,param }
  | PARAM ID EQ interval SCOL statements
    { let var,der,init,grd,jmp,param   = $6 in 
	var,der,init,grd,jmp,((mk_param $2 $4)::param) }

  | { (dummy_list,dummy_list,dummy_list,dummy_grd,dummy_list,[]) }
;

solver_params :
  | ID EQ float SCOL solver_params
    { set_param $1 $3 }
  | { }

/**/

expr_vec :
  | expr { [$1] }
  | expr expr_vec_rest { $1::$2 }
  | LP expr expr_vec_rest RP { $2::$3 }
;
expr_vec_rest :
  | COM expr expr_vec_rest
    { $2::$3 }
  | { [] }
;

var_vec :
  | ID var_vec_rest { (mk_var $1)::$2 }
  | { [] }
;
var_vec_rest :
  | COM ID var_vec_rest { (mk_var $2)::$3 }
  | { [] }
;
var_vec_old :
  | ID var_vec { (mk_var $1)::$2 }
  | { [] }
;

interval_vec :
  | interval interval_vec_rest { $1::$2 }
  | LP interval interval_vec_rest RP { $2::$3 }
;
interval_vec_rest :
  | COM interval interval_vec_rest
    { $2::$3 }
  | { [] }
;

/**/

expr :
  | expr PLUS term  { mk_expr (Papp2 (Oadd,$1,$3)) }
  | expr MIN term { mk_expr (Papp2 (Osub,$1,$3)) }
  | term { $1 }
;

term :
  | term MUL factor { mk_expr (Papp2 (Omul,$1,$3)) }
  | term DIV factor { mk_expr (Papp2 (Odiv,$1,$3)) }
  | factor { $1 }
;

factor :
  | LP expr RP { $2 }
  | factor POW integer
    { match $3 with 2 -> mk_expr (Papp (Osqr,$1)) | _ -> mk_expr (Papp2 (Opow,$1,(mk_expr (Pint $3)))) }
  | SQRT factor { mk_expr (Papp (Osqrt,$2)) }
  | EXP factor { mk_expr (Papp (Oexp,$2)) }
  | LOG factor { mk_expr (Papp (Olog,$2)) }
  | SIN factor { mk_expr (Papp (Osin,$2)) }
  | COS factor { mk_expr (Papp (Ocos,$2)) }
  | ATAN factor { mk_expr (Papp (Oatan,$2)) }
  | ASIN factor { mk_expr (Papp (Oasin,$2)) }
  | ACOS factor { mk_expr (Papp (Oacos,$2)) }
  | ID { mk_expr (Pvar $1) }
  /*| float { mk_expr (Pval $1) }*/
  | interval { mk_expr (Pval $1) }
  | MIN factor { mk_expr (Papp2 (Osub,(mk_expr (Pval (Point 0.))),$2)) }
;

rational :
  | integer { mk_ratio $1 1 }
  | integer DIV integer { mk_ratio $1 $3 }
;
integer :
  | INT { $1 }
  | MIN INT { -$2 }
;
float :
  | FLOAT { $1 }
  | MIN FLOAT { -.$2 }
  | integer { float_of_int $1 }
;
interval :
  | LB float COM float RB { Interval ($2,$4) }
  | float { Point $1 }
;
