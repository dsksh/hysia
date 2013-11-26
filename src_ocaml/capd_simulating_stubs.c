/**
 * function stubs for the simulation using CAPD
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

/*#include "capd_integrator.h"*/
#include "build.h"
#include "simulate.h"

#define LOG_DEBUG(msg, sig) 1
/*#define LOG_DEBUG(msg, sig) fprintf(stderr, "[capd_stubs] %s: %d\n", msg, sig)*/

value sim_initialize()
{
	CAMLparam0();
	simInitialize();
	CAMLreturn(Val_unit);
}

value sim_dispose()
{
	CAMLparam0();
	simDispose();
	CAMLreturn(Val_unit);
}

/*value sim_find_first_zero(value lid)
{
	CAMLparam1(lid);
	int res = findFirstZero(String_val(lid));
	CAMLreturn(Val_bool(res));
}

value sim_find_first_zero_mid(value lid)
{
	CAMLparam1(lid);
	int res = findFirstZeroMid(String_val(lid));
	CAMLreturn(Val_bool(res));
}
*/

value sim_find_first_zero(value do_print, value lid, value dst)
{
	CAMLparam3(do_print, lid, dst);
    CAMLlocal1(intv);

	cInterval res = findFirstZero(Bool_val(do_print), String_val(lid), String_val(dst));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.l));
    Store_field(intv, 1, caml_copy_double(res.u));
	CAMLreturn(intv);
}

value sim_find_first_zero_mid(value lid, value dst)
{
	CAMLparam2(lid, dst);
	int res = findFirstZeroMid(String_val(lid), String_val(dst));
	CAMLreturn(Val_bool(res));
}

value sim_simulate_jump(value lid, value dst, value zero_l, value zero_u)
{
	CAMLparam4(lid, dst, zero_l, zero_u);
	cInterval intv = { Double_val(zero_l), Double_val(zero_u) };
	simulateJump(String_val(lid), String_val(dst), intv);
	CAMLreturn(Val_unit);
}

value sim_print_pped(value is_lb, value is_last)
{
	CAMLparam2(is_lb, is_last);
	printPped(Bool_val(is_lb), Bool_val(is_last));
	CAMLreturn(Val_unit);
}
