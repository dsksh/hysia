%{
  open Ptree

  let env = ref SM.empty
  let set_param id v = env := SM.add id v !env

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr nd = loc (), nd
  let mk_ratio n d = loc (), (n,d)

  let mk_vf args es = loc (), (args,es)
  let mk_iv time vs = loc (), (time,vs)
  let mk_param id v = loc (), (id, v)
  let mk_var id = loc (), id

  let mk_model nd = (loc (), nd), !env
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token EQ
%token LP
%token RP
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

%token FUN
%token VAL
%token PARAM

%token EOF

%start main
%type <Ptree.model> main

%%

main :
  | statements solver_params { mk_model $1 }
; 

/**/

statements :
  | FUN var_vec EQ expr_vec SCOL statements
    { let _,vs,ps = $6 in (mk_vf $2 $4),vs,ps }
  | VAL integer EQ ratio_vec SCOL statements
    { let fs,_,ps = $6 in fs,(mk_iv $2 $4),ps }
  | PARAM ID EQ float SCOL statements
    { let fs,vs,ps = $6 in (fs,vs,(mk_param $2 $4)::ps) }
  | { (dummy_vf,dummy_iv,[]) }
;

solver_params :
  | ID EQ float SCOL solver_params
    { set_param $1 $3 }
  | { }

/**/

expr_vec :
  | expr { [$1] }
  | LP expr expr_vec_rest RP { $2::$3 }
;
expr_vec_rest :
  | COM expr expr_vec_rest
    { $2::$3 }
  | { [] }
;

var_vec :
  | ID var_vec { (mk_var $1)::$2 }
  | { [] }
;

ratio_vec :
  | LP float ratio_vec_rest RP
    { $2::$3 }
;
ratio_vec_rest :
  | COM float ratio_vec_rest
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
  | float { mk_expr (Pval $1) }
  | MIN factor { mk_expr (Papp2 (Osub,(mk_expr (Pval 0.)),$2)) }
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
