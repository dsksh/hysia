#ifndef _CAPD_UTIL_H_
#define _CAPD_UTIL_H_ 

#include <iostream>

#include <sys/time.h>

#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"
#include "capd/geomset/AffineSet.h"

#include "Parallelepiped.h"

namespace capd{ 

//#define INFINITY HUGE_VAL
#define UNIVERSE interval(INFINITY, INFINITY)

//#define HSS_PRINT_FREQ 72
//#define HSS_DUMP_PPED
//#define HSS_DEBUG

extern std::ostream cnull;


//#define HSS_DUMP_FILENAME "pped.dat"
//#define HSS_CONF_FILENAME "hss.conf"

//#define HSS_TIME_MAX 1e1
//#define HSS_QR_THRESHOLD 100

//#define HSS_BOX_BASED
//#define HSS_SKIP_PPED_T_INF


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


/// printing functions
inline void printBegin(std::ostream& out)
{
	out << '[' << std::endl;
}

inline void printEnd(std::ostream& out)
{
	out << ']' << std::endl;
}

inline void printStep(std::ostream& out, const int stepId, const char *lid, const double sim_time)
{
	//out << "(* step " << stepId << " at " << lid << ", time: (" << sim_time << ", " << (getTime()/1000.) << ") *)" << std::endl;
	std::cout << "step " << stepId << " at " << lid << ", time: " << sim_time << " (sim), " << (getTime()/1000.) << " (exec)" << std::endl;
}

inline void printInterval(std::ostream& out, const capd::interval& value)
{
	out << '[' << value.leftBound() << ',' << value.rightBound() << ']';
	out.flush();
}


/// printPped

template<typename Piped>
inline void printPped(std::ostream& out, const Piped& value);

template<>
inline void printPped(std::ostream& out, const capd::IVector& value) {

	out << "{ \"kind\":\"box\"," << std::endl;
	out << "  \"value\":[" << std::endl;

	// each axis
	for (int i(0); i < value.size(); ++i) {
		if (i>0) out << ", " << std::endl;
		printInterval(out, value[i]);
	}

	out << " ] }" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, const Parallelepiped& value) {

	out << "{ \"kind\":\"pped\"," << std::endl;
	out << "  \"value\":" << std::endl;

	// x
	out << "  { \"x\":[" << std::endl;
	bool first(true);
	for (int i(0); i < value.x().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.x().leftBound() << ',' << value.x().rightBound() << '}';
		out << value.x()[i].leftBound();
	}
	out << std::endl << "    ]," << std::endl;

	// B
	out << "    \"B\":[ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.A());
	for (int i(0); i < value.A().numberOfRows(); ++i) {
		it = value.A().beginOfRow(i+1);
		if (i >= 1) out << ',' << std::endl;
		out << '[';
		for (int j(0); j < value.A().numberOfColumns(); ++j) {
			if (j >= 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
			it.moveToNextColumn();
		}
		out << ']';
	}
	out << std::endl << "    ]," << std::endl;

	// r
	out << "    \"r\":[" << std::endl;
	first = true;
	for (int i(0); i < value.u().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '[' << value.u()[i].leftBound() << ',' << value.u()[i].rightBound() << ']';
	}
	out << std::endl << "    ]" << std::endl;
	out << "  }" << std::endl;

	out << "}" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, 
					  const capd::geomset::AffineSet<IMatrix>& value)
{
	out << "{ \"kind\":\"pped\"," << std::endl;
	out << "  \"value\":" << std::endl;

	// x
	out << "  { \"x\":[" << std::endl;
	bool first(true);
	for (int i(0); i < value.get_x().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.get_x()[i].leftBound() << ',' << value.get_x()[i].rightBound() << '}';
		out << value.get_x()[i].leftBound();
	}
	out << std::endl << "    ]," << std::endl;

	// B
	out << "    \"B\":[" << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.get_B());
	for (int i(1); i <= value.get_B().numberOfRows(); ++i) {
		it = value.get_B().beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '[';
		for (int j(1); j <= value.get_B().numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << ']';
	}
	out << std::endl << "    ]," << std::endl;

	// r
	out << "    \"r\":[" << std::endl;
	first = true;
	for (int i(0); i < value.get_r().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '[' << value.get_r()[i].leftBound() << ',' << value.get_r()[i].rightBound() << ']';
	}
	out << std::endl << "    ]" << std::endl;
	out << std::endl << "  }" << std::endl;

	out << std::endl << "}" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, 
					  const capd::C0Rect2Set& value)
{
	out << "{ \"kind\":\"pped\"," << std::endl;
	out << "  \"value\":" << std::endl;

	// x
	out << "  { \"x\":[" << std::endl;
	bool first(true);
	for (int i(0); i < value.get_x().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.get_x()[i].leftBound() << ',' << value.get_x()[i].rightBound() << '}';
		out << value.get_x()[i].leftBound();
	}
	out << std::endl << "    ]," << std::endl;

	// B
	out << "    \"B\":[" << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.get_B());
	for (int i(1); i <= value.get_B().numberOfRows(); ++i) {
		it = value.get_B().beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '[';
		for (int j(1); j <= value.get_B().numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << ']';
	}
	out << std::endl << "    ]," << std::endl;

	// r
	out << "    \"r\":[" << std::endl;
	first = true;
	for (int i(0); i < value.get_r().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '[' << value.get_r()[i].leftBound() << ',' << value.get_r()[i].rightBound() << ']';
	}
	out << std::endl << "    ]" << std::endl;
	out << std::endl << "  }" << std::endl;

	out << std::endl << "}" << std::endl;
	out.flush();
}


/// dumpPipe

template<typename Piped>
inline void printPipe(std::ostream& out, const capd::interval& time, const Piped& value)
//inline void printPipe(std::ostream& out,
//					  const capd::interval& time, const capd::C0Rect2Set& value)
{
	out << "{ \"time\":";

	// time
	printInterval(out, time);

	out << "," << std::endl;
	out << "  \"state\":" << std::endl;

	// each axis
	printPped(out, value);

	out << "}" << std::endl;
	out.flush();
}

} // the end of the namespace capd

#endif // _CAPD_UTIL_H_
