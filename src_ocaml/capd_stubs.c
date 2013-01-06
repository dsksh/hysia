/**
 * function stubs that call CAPD
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include "capd_integrator.h"
#include "nodebuilder.h"

#define LOG_DEBUG(msg, sig) 1
/*#define LOG_DEBUG(msg, sig) fprintf(stderr, "[capd_stubs] %s: %d\n", msg, sig)*/


value caml_test()
{
	CAMLparam0();
	test("");
	CAMLreturn(Val_unit);
}

value caml_test1(value command)
{
	CAMLparam1(command);
	test(String_val(command));
	CAMLreturn(Val_unit);
}


value caml_init(value dim)
{
	CAMLparam1(dim);
	init(Int_val(dim));
	CAMLreturn(Val_unit);
}

value caml_put_variable(value name)
{
	CAMLparam1(name);
	int index = putVariable(String_val(name));
	CAMLreturn(Val_int(index));
}

value caml_set_param(value id, value l, value u)
{
	CAMLparam3(id, l, u);
	int index = setParam(String_val(id), Double_val(l), Double_val(u));
	CAMLreturn(Val_int(index));
}

/*value caml_put_value(value l, value u)
{
	CAMLparam2(l, u);
	putValue(Double_val(l), Double_val(u));
	CAMLreturn(Val_unit);
}
*/

value caml_put_value()
{
	CAMLparam0();
	putValue();
	CAMLreturn(Val_unit);
}

value caml_put_var_node(value index)
{
	CAMLparam1(index);
	putVarNode(Int_val(index));
	CAMLreturn(Val_unit);
}

value caml_put_scalar_node(value l, value u)
{
	CAMLparam2(l, u);
	putScalarNode(Double_val(l), Double_val(u));
	CAMLreturn(Val_unit);
}

value caml_put_sqr_node()
{
	CAMLparam0();
	putSqrNode();
	CAMLreturn(Val_unit);
}

value caml_put_sqrt_node()
{
	CAMLparam0();
	putSqrtNode();
	CAMLreturn(Val_unit);
}

value caml_put_exp_node()
{
	CAMLparam0();
	putExpNode();
	CAMLreturn(Val_unit);
}

value caml_put_log_node()
{
	CAMLparam0();
	putLogNode();
	CAMLreturn(Val_unit);
}

value caml_put_sin_node()
{
	CAMLparam0();
	putSinNode();
	CAMLreturn(Val_unit);
}

value caml_put_cos_node()
{
	CAMLparam0();
	putCosNode();
	CAMLreturn(Val_unit);
}

value caml_put_atan_node()
{
	CAMLparam0();
	putAtanNode();
	CAMLreturn(Val_unit);
}

value caml_put_asin_node()
{
	CAMLparam0();
	putAsinNode();
	CAMLreturn(Val_unit);
}

value caml_put_acos_node()
{
	CAMLparam0();
	putAcosNode();
	CAMLreturn(Val_unit);
}

value caml_put_sum_node()
{
	CAMLparam0();
	putSumNode();
	CAMLreturn(Val_unit);
}

value caml_put_dif_node()
{
	CAMLparam0();
	putDifNode();
	CAMLreturn(Val_unit);
}

value caml_put_mul_node()
{
	CAMLparam0();
	putMulNode();
	CAMLreturn(Val_unit);
}

value caml_put_div_node()
{
	CAMLparam0();
	putDivNode();
	CAMLreturn(Val_unit);
}

value caml_put_pow_node()
{
	CAMLparam0();
	putPowNode();
	CAMLreturn(Val_unit);
}

value caml_put_der_tree(value i)
{
	CAMLparam1(i);
	putDerTree(Int_val(i));
	CAMLreturn(Val_unit);
}

value caml_put_der_dtree(value i, value j)
{
	CAMLparam2(i, j);
	putDerDTree(Int_val(i), Int_val(j));
	CAMLreturn(Val_unit);
}

value caml_done_der_tree()
{
	CAMLparam0();
	doneDerTree();
	CAMLreturn(Val_unit);
}

value caml_put_grd_tree()
{
	CAMLparam0();
	putGrdTree();
	CAMLreturn(Val_unit);
}

value caml_put_grd_dtree(value j)
{
	CAMLparam1(j);
	putGrdDTree(Int_val(j));
	CAMLreturn(Val_unit);
}

value caml_put_jump_tree(value i)
{
	CAMLparam1(i);
	putJumpTree(Int_val(i));
	CAMLreturn(Val_unit);
}

value caml_put_jump_dtree(value i, value j)
{
	CAMLparam2(i, j);
	putJumpDTree(Int_val(i), Int_val(j));
	CAMLreturn(Val_unit);
}
