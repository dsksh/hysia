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

/*value sim_check_inv(value lid, value iid)
{
	CAMLparam2(lid, iid);
	int res = checkInvAtInitTime(String_val(lid), Int_val(iid));
	CAMLreturn(Val_bool(res));
}*/

value sim_check_prop(value lid, value apid)
{
	CAMLparam2(lid, apid);
	int res = checkPropAtInitTime(String_val(lid), Int_val(apid));
	CAMLreturn(Val_int(res));
}

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

value sim_check_prop_kind(value lid, value apid)
{
	CAMLparam2(lid, apid);
    CAMLlocal2(intv, pair);

	double vl, vu;
	int res = checkPropKind(String_val(lid), Int_val(apid), &vl, &vu);

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(vl));
    Store_field(intv, 1, caml_copy_double(vu));

    pair = caml_alloc(2, 0);
    Store_field(pair, 0, Val_int(res));
    Store_field(pair, 1, intv);

	CAMLreturn(pair);
}

value sim_find_prop_extremum(value lid, value apid, value time_lower, value time_max)
{
	CAMLparam4(lid, apid, time_lower, time_max);
    CAMLlocal1(intv);

	cInterval res = findPropExtremum(String_val(lid), Int_val(apid),
									 Double_val(time_lower), Double_val(time_max));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.l));
    Store_field(intv, 1, caml_copy_double(res.u));
	CAMLreturn(intv);
}

value sim_compare_signals(value lid, value neg1, value neg2, value st1, value st2, value apid1, value apid2, value time_lower, value time_max)
{
	CAMLparam5(lid, neg1, neg2, st1, st2);
	CAMLxparam4(apid1, apid2, time_lower, time_max);
    CAMLlocal2(intv, pair);

	cSigComp res = compareSignals(String_val(lid), Bool_val(neg1), Bool_val(neg2),
								  Double_val(st1), Double_val(st2),
								  Int_val(apid1), Int_val(apid2),
								  Double_val(time_lower), Double_val(time_max));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.intv.l));
    Store_field(intv, 1, caml_copy_double(res.intv.u));

    pair = caml_alloc(2, 0);
    Store_field(pair, 0, Val_int(res.apid));
    Store_field(pair, 1, intv);
	CAMLreturn(pair);
}

value sim_find_intersection(value lid, value neg, value st, value apid, value vl, value vu, value time_lower, value time_max)
{
	CAMLparam4(lid, neg, st, apid);
	CAMLxparam4(vl, vu, time_lower, time_max);
    CAMLlocal2(intv, pair);

	cSigComp res = findIntersection(String_val(lid), Bool_val(neg), Double_val(st), Int_val(apid), 
								    Double_val(vl), Double_val(vu),
								    Double_val(time_lower), Double_val(time_max) );

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.intv.l));
    Store_field(intv, 1, caml_copy_double(res.intv.u));

    pair = caml_alloc(2, 0);
    Store_field(pair, 0, Val_int(res.apid));
    Store_field(pair, 1, intv);
	CAMLreturn(pair);
}

value sim_simulate_jump(value lid, value eid, value zero_l, value zero_u)
{
	CAMLparam4(lid, eid, zero_l, zero_u);
	cInterval intv;
	intv.l = Double_val(zero_l); intv.u = Double_val(zero_u);
	simulateJump(String_val(lid), Int_val(eid), intv);
	CAMLreturn(Val_unit);
}

value sim_simulate_cont(value lid, value time_max)
{
	CAMLparam2(lid, time_max);
	simulateCont(String_val(lid), Double_val(time_max));
	CAMLreturn(Val_unit);
}

value sim_value_at(value t, value is_neg, value lid, value apid)
{
	CAMLparam4(t, is_neg, lid, apid);
    CAMLlocal1(intv);

	cInterval res = valueAt(Double_val(t), Bool_val(is_neg), String_val(lid), Int_val(apid));

    intv = caml_alloc(2, 0);
    Store_field(intv, 0, caml_copy_double(res.l));
    Store_field(intv, 1, caml_copy_double(res.u));

	CAMLreturn(intv);
}

value sim_dump_const(value is_neg, value vl, value vu, value time_lower, value time_max)
{
	CAMLparam5(is_neg, vl, vu, time_lower, time_max);
	dumpConst(Bool_val(is_neg), Double_val(vl), Double_val(vu), Double_val(time_lower), Double_val(time_max));
	CAMLreturn(Val_unit);
}

value sim_dump_ap(value lid, value apid, value is_neg, value st, value time_lower, value time_max)
{
	CAMLparam4(lid, apid, is_neg, st);
	CAMLxparam2(time_lower, time_max);
	dumpAP(String_val(lid), Int_val(apid), Bool_val(is_neg), Double_val(st), Double_val(time_lower), Double_val(time_max));
	CAMLreturn(Val_unit);
}

value sim_dump_bool(value is_neg, value tl, value tu)
{
	CAMLparam3(is_neg, tl, tu);
	dumpBool(Bool_val(is_neg), Double_val(tl), Double_val(tu));
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

value sim_get_dump_data()
{
	CAMLparam0();
	CAMLlocal1(os);
	char *s;
	s = getDumpData();
	if (s != NULL) {
		os = caml_copy_string(s);
		free(s);
	} else
		os = caml_copy_string("");
	CAMLreturn(os);
}
