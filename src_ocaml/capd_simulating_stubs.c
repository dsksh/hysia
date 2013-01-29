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

value sim_find_first_zero()
{
	CAMLparam0();
	int res = findFirstZero();
	CAMLreturn(Val_bool(res));
}

value sim_find_first_zero_mid()
{
	CAMLparam0();
	int res = findFirstZeroMid();
	CAMLreturn(Val_bool(res));
}

value sim_simulate_jump()
{
	CAMLparam0();
	simulateJump();
	CAMLreturn(Val_unit);
}

value sim_print_pped(value is_lb, value is_last)
{
	CAMLparam2(is_lb, is_last);
	printPped(Bool_val(is_lb), Bool_val(is_last));
	CAMLreturn(Val_unit);
}
