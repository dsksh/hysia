#ifndef _CAPD_UTIL_H_
#define _CAPD_UTIL_H_ 

#include <exception>
#include <setjmp.h>
#include <sys/time.h>

#include <iostream>

#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"
#include "capd/geomset/AffineSet.h"

#include "Parallelepiped.h"
#include "PrintDriver.h"

namespace capd { 

//#define INFINITY HUGE_VAL
#define UNIVERSE interval(INFINITY, INFINITY)

//#define HSS_PRINT_FREQ 72
//#define HSS_DUMP_PPED
//#define HSS_DEBUG

extern std::ostream cnull;


/// timer

static struct timeval tstart;

inline void startTimer()
{
    gettimeofday(&tstart, NULL);
}

inline unsigned long long getTime()
{
    struct timeval tfinish;
    long sec, usec;

    gettimeofday(&tfinish, NULL);
    sec = tfinish.tv_sec - tstart.tv_sec;
    usec = tfinish.tv_usec - tstart.tv_usec;
    return 1e+6*sec + usec;
}

//

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


/// interval operators

/// extended division on intervals
/// Prop.4.3.1 of [Neumaier 1990]
inline void extDiv(const capd::interval& numerator, const capd::interval& denominator, 
			capd::interval *&domain, capd::interval *&containment) 
{
//std::cout << "extDiv: " << numerator << " / " << denominator << " dom: " << *domain << std::endl;
	containment = NULL;
	if (!denominator.contains(capd::TypeTraits<capd::interval>::zero())) {
//std::cout << numerator / denominator << " CAP " << *domain << std::endl;
		if (!intersection(numerator / denominator, *domain, *domain)) {
			domain = NULL;
		}
	}
	else if (!numerator.contains((capd::TypeTraits<capd::interval>::zero()))) {
//std::cout << numerator << " / " << denominator << std::endl;

		capd::interval no_domain(0);
		if (numerator > capd::TypeTraits<capd::interval>::zero()) {
			no_domain = intervalHull(
				denominator.left() != 0
				? (numerator.left() / denominator.left()) : -INFINITY,
				denominator.right() != 0
				? (numerator.left() / denominator.right()) : INFINITY);
		}
		else if (numerator < capd::TypeTraits<capd::interval>::zero()) {
			no_domain = intervalHull(
				denominator.right() != 0
				? (numerator.right() / denominator.right()) : -INFINITY,
				denominator.left() != 0
				? (numerator.right() / denominator.left()) : INFINITY);
		}

		//if (!intersection(*domain, no_domain, no_domain))
		//	return;

//std::cout << no_domain << std::endl;

		// no intersection
		if (domain->rightBound() < no_domain.leftBound() ||
			domain->leftBound()  > no_domain.rightBound()) {

			return;
		}

		if (domain->rightBound() >= no_domain.rightBound()) {

			if (domain->leftBound() > no_domain.leftBound()) {

				domain->setLeftBound(no_domain.rightBound());
			}
			else {
				capd::interval bak(*domain);
				domain->setRightBound(no_domain.leftBound());
				containment = new capd::interval(bak);
				containment->setLeftBound(no_domain.rightBound());
			}
		}
		else { // domain->rightBound() < no_domain.rightBound()

			if (domain->leftBound() <= no_domain.leftBound()) {

				domain->setRightBound(no_domain.rightBound());
			}
			else {
				domain = NULL;
			}
		}
	}

	// do nothing when numerator contains zero
}

/// Hausdorff distance
inline double hausdorff(const capd::interval& I1, const capd::interval& I2) 
{	
	if (std::isinf(I1.leftBound()) || std::isinf(I1.rightBound()) || 
		std::isinf(I2.leftBound()) || std::isinf(I2.rightBound())) { 

		if (I1 == UNIVERSE && I2 == UNIVERSE) {
			// hausdorff([-oo, +oo],[-oo,+oo]) = 0	
			return 0.0;
		}
		else {	
			return INFINITY;
		}
	} else {	
		return std::max(std::fabs(I1.leftBound() -I2.leftBound()),
						std::fabs(I1.rightBound()-I2.rightBound()));
	}
}	


/// printing functions

inline void printBegin(std::ostream& out)
{
	g_print_dvr->printBegin(out);
}
inline void printEnd(std::ostream& out)
{
	g_print_dvr->printEnd(out);
}
inline void printStep(std::ostream& out, const int stepId, const char *lid, const double sim_time)
{
	g_print_dvr->printStep(out, stepId, lid, sim_time);
}
inline void printInterval(std::ostream& out, const capd::interval& value)
{
	g_print_dvr->printInterval(out, value);
}


/// printPped

template<typename Pped>
inline void printPped(std::ostream& out, const Pped& value);

template<>
inline void printPped(std::ostream& out, const capd::IVector& value) 
{
	g_print_dvr->printBox(out, value);
}

template<>
inline void printPped(std::ostream& out, const Parallelepiped& value) 
{
	g_print_dvr->printPped(out, value.x(), value.A(), value.u());
}

template<>
inline void printPped(std::ostream& out, 
					  const capd::geomset::AffineSet<IMatrix>& value)
{
	g_print_dvr->printPped(out, value.get_x(), value.get_B(), value.get_r());

}

template<>
inline void printPped(std::ostream& out, const capd::C0Rect2Set& value)
{
	g_print_dvr->printPped(out, value.get_x(), value.get_B(), value.get_r());
}


/// dumpPipe

template<typename Pped>
inline void printPipe(std::ostream& out, const capd::interval& time, const Pped& value)
{
	g_print_dvr->prologuePipe(out, time);
	printPped<Pped>(out, value);
	g_print_dvr->epiloguePipe(out);
}


} // the end of the namespace capd

#endif // _CAPD_UTIL_H_
