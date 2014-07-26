%{
  open Model_common
  open Ptree

  let env = ref MParam.empty
  let set_param id v = env := MParam.add id v !env

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr nd = loc (), nd
  let mk_ratio n d = loc (), (n,d)

  let mk_id id = loc (), id
  let mk_id_l v = loc (), v
  let mk_iparam id v = loc (), (id, PVint v)
  let mk_rparam id v = loc (), (id, PVrandom v)
  let mk_expr_l e = loc (), e
  let mk_init v = loc (), v
  let mk_loc id der inv edges = loc (), (id,der,inv,edges)
  let mk_edge gh gg dst rst = loc (), (gh,gg,dst,rst)
  let mk_edge_l e = loc (), e

  let mk_ptree nd = nd, !env
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token TRUE
%token EQ
%token LP
/*%token LP_STAR_RP*/
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
%token INIT
%token FINAL
%token AT
%token INV
%token WAIT
%token FLOW
%token END
%token LET
%token RANDOM
%token WATCH
%token GOTO
%token THEN

%token PARAM

%token EOF

%start main
%type <Ptree.t * float Model_common.MParam.t> main

%%

main :
  | statements solver_params { mk_ptree $1 }
; 

/**/

statements :
  | VAR var_vec statements
    { let ps,_,init,final,locs = $3 in 
	    ps,(mk_id_l $2),init,final,locs }
  | INIT expr_vec statements
    { let ps,vs,_,final,locs = $3 in 
	    ps,vs,(mk_init $2),final,locs }
  | FINAL expr_vec statements
    { let ps,vs,init,_,locs = $3 in 
	    ps,vs,init,(mk_expr_l $2),locs }
  | AT ID WAIT expr_vec INV expr_vec edges END statements
    { let ps,vs,init,final,locs = $9 in 
        ps,vs,init,final,(mk_loc (mk_id $2) (mk_expr_l $4) (mk_expr_l $6) (mk_edge_l $7))::locs }
  | AT ID FLOW expr_vec INV expr_vec edges END statements
    { let ps,vs,init,final,locs = $9 in 
        ps,vs,init,final,(mk_loc (mk_id $2) (mk_expr_l $4) (mk_expr_l $6) (mk_edge_l $7))::locs }

  | LET ID EQ interval statements
    { let ps,vs,init,final,locs = $5 in 
        (mk_iparam $2 $4)::ps,vs,init,final,locs }
  | LET ID EQ RANDOM float statements
    { let ps,vs,init,final,locs = $6 in 
        (mk_rparam $2 $5)::ps,vs,init,final,locs }

  | { ([],dummy_list,dummy_list,dummy_list,[]) }
;

solver_params :
  | PARAM ID EQ float solver_params
    { set_param $2 $4 }
  | { }
;

/**/

edges :
  /*| WATCH LP expr COM expr_vec RP GOTO ID edges
    { (mk_edge $3 $5 (mk_id $8) (mk_expr_l []))::$9 }
  */
  | WATCH LP expr COM expr_vec RP GOTO ID THEN expr_vec edges
    { (mk_edge $3 (mk_expr_l $5) (mk_id $8) (mk_expr_l $10))::$11 }

  | { [] }
;

/**/

expr_vec :
  | TRUE { [] }
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
  | ID var_vec_rest { (mk_id $1)::$2 }
  | { [] }
;
var_vec_rest :
  | COM ID var_vec_rest { (mk_id $2)::$3 }
  | { [] }
;
var_vec_old :
  | ID var_vec { (mk_id $1)::$2 }
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
