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

#include "capd_integrator.h"
#include "nodebuilder.h"
#include "firstzero.h"

#define LOG_DEBUG(msg, sig) 1
/*#define LOG_DEBUG(msg, sig) fprintf(stderr, "[capd_stubs] %s: %d\n", msg, sig)*/

value sim_integrate(value a1, value a2, value a3, value a4)
{
	CAMLparam4(a1, a2, a3, a4);
	integrate(Double_val(a1), Double_val(a2), Double_val(a3), Double_val(a4));
	CAMLreturn(Val_unit);
}

value sim_initialize()
{
	CAMLparam0();
	simInitialize();
	CAMLreturn(Val_unit);
}

value sim_find_zero()
{
	CAMLparam0();
	int res = firstZero();
	CAMLreturn(Val_int(res));
}
