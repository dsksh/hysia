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
#include "sendingHandler.h"
#include "simulatingHandler.h"

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

value sim_set_param(value lid, value id, value v)
{
	CAMLparam3(lid, id, v);
	setParam(String_val(lid), String_val(id), Double_val(v));
	CAMLreturn(Val_unit);
}

/*value sim_check_prop(value lid, value apid)
{
	CAMLparam2(lid, apid);
	int res = checkProp(String_val(lid), Int_val(apid));
	CAMLreturn(Val_bool(res));
}
*/

value sim_find_inv_frontier(value lid, value iid)
{
	CAMLparam2(lid, iid);
    CAMLlocal1(intv);

	cInterval res = findInvFrontier(String_val(lid), Int_val(iid));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.l));
    Store_field(intv, 1, caml_copy_double(res.u));
	CAMLreturn(intv);
}

value sim_find_prop_frontier(value lid, value apid, value polar, value time_lower, value time_max)
{
	CAMLparam5(lid, apid, polar, time_lower, time_max);
    CAMLlocal1(intv);

	cInterval res = findPropFrontier(String_val(lid), Int_val(apid), Bool_val(polar),
									 Double_val(time_lower), Double_val(time_max));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.l));
    Store_field(intv, 1, caml_copy_double(res.u));
	CAMLreturn(intv);
}

value sim_find_first_zero(value do_print, value lid, value eid)
{
	CAMLparam3(do_print, lid, eid);
    CAMLlocal1(intv);

	cInterval res = findFirstZero(Bool_val(do_print), String_val(lid), Int_val(eid));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.l));
    Store_field(intv, 1, caml_copy_double(res.u));
	CAMLreturn(intv);
}

value sim_find_first_zero_mid(value lid, value eid)
{
	CAMLparam2(lid, eid);
	int res = findFirstZeroMid(String_val(lid), Int_val(eid));
	CAMLreturn(Val_bool(res));
}

value sim_simulate_jump(value lid, value eid, value zero_l, value zero_u)
{
	CAMLparam4(lid, eid, zero_l, zero_u);
	cInterval intv = { Double_val(zero_l), Double_val(zero_u) };
	simulateJump(String_val(lid), Int_val(eid), intv);
	CAMLreturn(Val_unit);
}

value sim_simulate_cont(value lid)
{
	CAMLparam1(lid);
	simulateCont(String_val(lid));
	CAMLreturn(Val_unit);
}

value sim_print_pped(value is_lb, value is_last)
{
	CAMLparam2(is_lb, is_last);
	printPped(Bool_val(is_lb), Bool_val(is_last));
	CAMLreturn(Val_unit);
}

value sim_report_step(value step_id, value lid)
{
	CAMLparam2(step_id, lid);
	reportStep(Int_val(step_id), String_val(lid));
	CAMLreturn(Val_unit);
}
