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

#define LOG_DEBUG(msg, sig) 1
//#define LOG_DEBUG(msg, sig) fprintf(stderr, "[capd_stubs] %s: %d\n", msg, sig)


void caml_test()
{
	integrate("");
}

value caml_test1(value command)
{
	CAMLparam1(command);

	integrate(String_val(command));

	CAMLreturn(Val_unit);
}
