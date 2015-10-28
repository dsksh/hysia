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
  let mk_edge f gh gg dst rst = loc (), (f,gh,gg,dst,rst)
  let mk_edge_l e = loc (), e

  let mk_ptree nd prop = (nd, prop), !env

  let mk_prop f = loc (), f
%}

/* tokens */

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

/* keywords */

%token VAR
%token INIT
%token AT
%token INV
%token WAIT
%token FLOW
%token END
%token LET
%token RANDOM
%token WHEN
%token ONCE
%token GOTO
%token THEN
%token PROP

/*%token LOC*/

%token PARAM

%token LB
%token RB
%token COM
%token SCOL

/* formula expression keywords */

%token TRUE
%token FALSE
%token EQ
%token LP
/*%token LP_STAR_RP*/
%token RP

%token EVENTLY
%token ALWAYS
%token UNTIL
%token NOT
%token AND
%token OR
%token IMP

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

%token EOF

/**/

%right IMP
%right AND
%right OR
%nonassoc NOT
%left PLUS MIN
%left MUL DIV
%nonassoc SQRT EXP LOG SIN COS ATAN ASIN ACOS
%nonassoc POW UMIN

/**/

%start main property
%type <Ptree.t * float Model_common.MParam.t> main
%type <Ptree.mitl_formula> property

%%

main :
  | statements property solver_params { mk_ptree $1 $2 }
; 

/**/

statements :
  | VAR var_list statements
    { let ps,_,init,locs = $3 in 
	    ps,(mk_id_l $2),init,locs }
  | INIT expr_list statements
    { let ps,vs,_,locs = $3 in 
	    ps,vs,(mk_init $2),locs }
  | AT ID flow invariant edges END statements
    { let ps,vs,init,locs = $7 in 
        ps,vs,init,(mk_loc (mk_id $2) $3 $4 (mk_edge_l $5))::locs }

  | LET ID EQ interval statements
    { let ps,vs,init,locs = $5 in 
        (mk_iparam $2 $4)::ps,vs,init,locs }
  | LET ID EQ RANDOM float statements
    { let ps,vs,init,locs = $6 in 
        (mk_rparam $2 $5)::ps,vs,init,locs }

  | { ([],dummy_list,dummy_list,[]) }
;

property :
  | PROP mitl_formula { mk_prop $2 }
  | { dummy_prop }
;

solver_params :
  | solver_params PARAM ID EQ float_pn
    { set_param $3 $5 }
  | { env := MParam.empty }
;

/**/

flow :
  | WAIT expr_list
    { mk_expr_l $2 }
  | FLOW expr_list
    { mk_expr_l $2 }
;

invariant :
  | { mk_expr_l [] }
  | INV expr_list
    { mk_expr_l $2 }
;

edges :
  | WHEN LP expr COM expr_list RP GOTO ID THEN expr_list edges
    { (mk_edge false $3 (mk_expr_l $5) (mk_id $8) (mk_expr_l $10))::$11 }
  | ONCE LP expr COM expr_list RP GOTO ID THEN expr_list edges
    { (mk_edge true $3 (mk_expr_l $5) (mk_id $8) (mk_expr_l $10))::$11 }

  | { [] }
;

/**/

expr_list :
  | TRUE { [] } /* TODO */
  | expr { [$1] }
  | TRUE expr_list_rest{ $2 }
  | expr expr_list_rest { $1::$2 }
  | LP expr expr_list_rest RP { $2::$3 }
;
expr_list_rest :
  | COM expr expr_list_rest
    { $2::$3 }
  | { [] }
;

var_list :
  | ID var_list_rest { (mk_id $1)::$2 }
  | { [] }
;
var_list_rest :
  | COM ID var_list_rest { (mk_id $2)::$3 }
  | { [] }
;
var_list_old :
  | ID var_list { (mk_id $1)::$2 }
  | { [] }
;

interval_list :
  | interval interval_list_rest { $1::$2 }
  | LP interval interval_list_rest RP { $2::$3 }
;
interval_list_rest :
  | COM interval interval_list_rest
    { $2::$3 }
  | { [] }
;

/**/

expr :
  | expr PLUS expr { mk_expr (Papp2 (Oadd,$1,$3)) }
  | expr MIN  expr { mk_expr (Papp2 (Osub,$1,$3)) }
  | expr MUL  expr { mk_expr (Papp2 (Omul,$1,$3)) }
  | expr DIV  expr { mk_expr (Papp2 (Odiv,$1,$3)) }
  | expr POW integer
    { match $3 with 
      | 2 -> mk_expr (Papp (Osqr,$1)) 
      | _ -> mk_expr (Papp2 (Opow,$1,(mk_expr (Pint $3)))) }
  | SQRT expr { mk_expr (Papp (Osqrt,$2)) }
  | EXP  expr { mk_expr (Papp (Oexp,$2)) }
  | LOG  expr { mk_expr (Papp (Olog,$2)) }
  | SIN  expr { mk_expr (Papp (Osin,$2)) }
  | COS  expr { mk_expr (Papp (Ocos,$2)) }
  | ATAN expr { mk_expr (Papp (Oatan,$2)) }
  | ASIN expr { mk_expr (Papp (Oasin,$2)) }
  | ACOS expr { mk_expr (Papp (Oacos,$2)) }
  | ID { mk_expr (Pvar $1) }
  | interval { mk_expr (Pval $1) }
  | MIN expr %prec UMIN { mk_expr (Papp2 (Osub,(mk_expr (Pval Interval.zero)),$2)) }
  | LP expr RP { $2 }
;

/**/

mitl_formula :
  | mitl_formula_term
    { $1 }
  | mitl_formula AND mitl_formula
    { Pand ($1,$3) }
    /*{ Pnot (Por ((Pnot $1), (Pnot $3))) }*/
  | mitl_formula OR mitl_formula
    /*{ Pnot (Pand ((Pnot $1), (Pnot $3))) }*/
    { Por ($1,$3) }
  | mitl_formula IMP mitl_formula
    /*{ Pnot (Pand ($1, Pnot $3)) }*/
    /*{ Por (Pnot $1, $3) }*/
    { Pimp ($1,$3) }
;

mitl_formula_term :
  | mitl_formula_term_sub until mitl_formula_term
    { Puntil ($2,$1,$3) }
  | mitl_formula_term_sub UNTIL mitl_formula_term
    { Puntil_ut ($1,$3) }
  | mitl_formula_term_sub
    { $1 }
;
mitl_formula_term_sub :
  | TRUE
    { Ptrue }
  | FALSE
    { Pnot Ptrue }
  /*| LOC ID*/
  | ID
    { Ploc $1 }
  | expr
    { Pexpr $1 }
  | NOT mitl_formula
    { Pnot $2 }
  | evently mitl_formula_term_sub
    /*{ Puntil ($1,Ptrue,$2) }*/
    { Pevt ($1,$2) }
  | EVENTLY mitl_formula_term_sub
    { Pevt_ut $2 }
  | always mitl_formula_term_sub
    /*{ Pnot (Puntil ($1,Ptrue,Pnot $2)) }*/
    { Palw ($1,$2) }
  | ALWAYS mitl_formula_term_sub
    { Palw_ut $2 }
  | LP mitl_formula RP
    { $2 }
;

always :
  | ALWAYS noun_interval { $2 }
;
evently :
  | EVENTLY noun_interval { $2 }
;
until :
  | UNTIL noun_interval { $2 }
;

/**/

/*rational :
  | integer { mk_ratio $1 1 }
  | integer DIV integer { mk_ratio $1 $3 }
;*/
integer :
  | INT { $1 }
  | MIN INT { -$2 }
;
float :
  | FLOAT { $1 }
  | integer { float_of_int $1 }
;
float_pn :
  | float { $1 }
  | MIN float { -.$2 }
; 
interval :
  | noun_interval { $1 }
  | float_pn { Interval.interval_of_float $1 }
;
noun_interval :
  | LB float_pn COM float_pn RB { {inf=$2; sup=$4} }
;
