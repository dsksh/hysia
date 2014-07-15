#include <iostream>

#include "Context.h"
#include "util.h"
#include "simulatingHandler.h"

namespace capd{ 

std::ostream cnull(0);

} // the end of the namespace capd

void printPped(int is_lb, int is_last)
{
	const capd::interval& time(is_lb ? g_context->time.left() : g_context->time.right());
	printPipe(g_context->fout, time, g_context->pped);
	if (!is_last)
		g_context->fout << ',' << std::endl;
}
