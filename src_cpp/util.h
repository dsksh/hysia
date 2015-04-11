#ifndef _CAPD_UTIL_H_
#define _CAPD_UTIL_H_ 

#include <exception>
#include <setjmp.h>

#include "utilJson.h"
//#include "utilMath.h"

//#define EXCEPTION_HACK 1

namespace capd { 

#if EXCEPTION_HACK
extern jmp_buf eh_jb;
extern std::exception eh_ex;
#endif

#if !EXCEPTION_HACK
#	define TRY try
#	define CATCH catch (const std::exception& eh_ex)
#	define THROW(msg) throw std::runtime_error(msg)
#else
	// emulation of exception handling to deal with a bug in Mac OS.
	// try ... catch() is also needed for the exceptions of CAPD etc.
#	define TRY if (setjmp(eh_jb) == 0) try
#	define CATCH catch (std::exception& e) { eh_ex = e; goto EH_HANDLER; } else EH_HANDLER:
#	define THROW(msg) do { eh_ex = std::runtime_error(msg); longjmp(eh_jb, 1); } while (0);
#endif

}

#endif // _CAPD_UTIL_H_
