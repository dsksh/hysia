%{
  open Model_common
  open Ptree
  open Ba_ptree

  let env = ref MParam.empty
  let set_param id v = env := MParam.add id v !env

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr nd = loc (), nd
  let mk_ratio n d = loc (), (n,d)

  let mk_ba_expr nd = loc (), nd

  let mk_edge lb dst = loc (), (lb,dst)

  let mk_loc lb es = loc (), (lb,es)
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%token EQ
%token GT
%token LT
%token GEQ
%token LEQ

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

%token TRUE 
%token FALSE

%token DEFINE
%token NEVER
%token IF
%token FI
%token ARROW
%token GOTO

%token SHARP

%token LP
%token RP
%token LB
%token RB
%token LSB
%token RSB
%token COL
%token SCOL
%token COM

%token NEG
%token AND
%token OR

%token EOF

%start main
%type <Ba_ptree.t> main

%%

main :
  | defines never_claim
    { $2 }
;

defines :
  | { }
  | SHARP DEFINE ID prop defines
    { (*define $3 $4*) }
;

never_claim :
  | NEVER LB nc_sequence RB
    { $3 }
;

nc_sequence :
  | nc_statement
    { [$1] }
  | nc_statement SCOL
    { [$1] }
  | nc_statement SCOL nc_sequence
    { $1::$3 }
;

nc_statement :
  | ID COL IF nc_options FI
    { mk_loc $1 $4 }
;

nc_options :
  | { [] }
  | COL COL nc_expr ARROW GOTO ID nc_options
    { (mk_edge $3 $6)::$7 }
;

nc_expr :
  | TRUE
    { mk_ba_expr (Pbool true) }
  | ID
    { mk_ba_expr (Pvar $1) }
  | LP nc_expr RP
    { $2 }
  | NEG nc_expr
    { mk_ba_expr (Papp (Oneg,$2)) }
  | nc_expr AND AND nc_expr
    { mk_ba_expr (Papp2 (Oand,$1,$4)) }
  | nc_expr OR OR nc_expr
    { mk_ba_expr (Papp2 (Oor,$1,$4)) }
;

/**/

prop :
  | TRUE
    { mk_ba_expr (Pbool true) }
  /*| conjunction
    {  }
  | conjunction OR prop
    {  }
  */
;

/**/

conjunction :
  | formula
    {  }
  | LP conjunction RP
    {  }
  | formula AND conjunction
    {  }
  ;

formula :
  | TRUE
    {  }
  | FALSE
    {  }
  | ID
    {  }
  ;

/*expr :
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
  | LSB float COM float RSB { Interval ($2,$4) }
  | float { Point $1 }
;
*/
