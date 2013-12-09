#ifndef _CAPD_UTIL_H_
#define _CAPD_UTIL_H_ 

#include <iostream>

#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"

#include "Parallelepiped.h"

namespace capd{ 

//#define INFINITY HUGE_VAL
#define UNIVERSE interval(INFINITY, INFINITY)

//#define HSS_PRINT_FREQ 72
#define HSS_DUMP_PPED
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
inline void extDiv(const interval& denominator, const interval& numerator, 
			interval **domain, interval **containment) 
{
	*containment = NULL;
	if (!denominator.contains(capd::TypeTraits<interval>::zero())) {
		if (!intersection(numerator / denominator, **domain, **domain)) {
			*domain = NULL;
		}
	}
	else if (!numerator.contains((capd::TypeTraits<interval>::zero()))) {
//std::cout << numerator << " / " << denominator << std::endl;

		interval no_domain(0);
		if (numerator > capd::TypeTraits<interval>::zero()) {
			no_domain = intervalHull(
				denominator.left() != 0
				? (numerator.left() / denominator.left()) : -INFINITY,
				denominator.right() != 0
				? (numerator.left() / denominator.right()) : INFINITY);
		}
		else if (numerator < capd::TypeTraits<interval>::zero()) {
			no_domain = intervalHull(
				denominator.right() != 0
				? (numerator.right() / denominator.right()) : -INFINITY,
				denominator.left() != 0
				? (numerator.right() / denominator.left()) : INFINITY);
		}

		//if (!intersection(**domain, no_domain, no_domain))
		//	return;

//std::cout << no_domain << std::endl;

		if ((*domain)->rightBound() <= no_domain.leftBound() ||
			(*domain)->leftBound()  >= no_domain.rightBound()) {

			return;
		}

		if ((*domain)->rightBound() >= no_domain.rightBound()) {

			if ((*domain)->leftBound() > no_domain.leftBound()) {

				(*domain)->setLeftBound(no_domain.rightBound());
			}
			else {
				interval bak(**domain);
				(*domain)->setRightBound(no_domain.leftBound());
				*containment = new interval(bak);
				(*containment)->setLeftBound(no_domain.rightBound());
			}
		}
		else { // (*domain)->rightBound() < no_domain.rightBound()

			if ((*domain)->leftBound() <= no_domain.leftBound()) {

				(*domain)->setRightBound(no_domain.rightBound());
			}
			else {
				*domain = NULL;
			}
		}

		/*
		interval bak(**domain);
		(*domain)->setRightBound(no_domain.leftBound());

		if (bak.rightBound() >= no_domain.rightBound()) {
			*containment = new interval(bak);
			(*containment)->setLeftBound(no_domain.rightBound());
		}
		}
		else if ((*domain)->rightBound() >= no_domain.rightBound())
			(*domain)->setLeftBound(no_domain.rightBound());
		else
			*domain = NULL;
		*/
	}

	// do nothing when numerator contains zero
}

/// Hausdorff distance
inline double hausdorff(const interval& I1, const interval& I2) 
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

inline void printInterval(std::ostream& out, const interval& value)
{
	out << '{' << value.leftBound() << ',' << value.rightBound() << '}';
	out.flush();
}


/// printPped

template<typename Piped>
inline void printPped(std::ostream& out, const Piped& value);

template<>
inline void printPped(std::ostream& out, const capd::IVector& value) {

	out << "box[{" << std::endl;

	// each axis
	for (int i(0); i < value.size(); ++i) {
		if (i>0) out << ", " << std::endl;
		printInterval(out, value[i]);
	}

	out << "}]" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, const Parallelepiped& value) {

	out << "pped[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.x().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.x().leftBound() << ',' << value.x().rightBound() << '}';
		out << value.x()[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.A());
	for (int i(0); i < value.A().numberOfRows(); ++i) {
		it = value.A().beginOfRow(i+1);
		if (i >= 1) out << ',' << std::endl;
		out << '{';
		for (int j(0); j < value.A().numberOfColumns(); ++j) {
			if (j >= 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
			it.moveToNextColumn();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.u().size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.u()[i].leftBound() << ',' << value.u()[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << "]" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, 
					  const capd::C0Rect2Set& value)
{
	out << "pped[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.m_x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.m_x[i].leftBound() << ',' << value.m_x[i].rightBound() << '}';
		out << value.m_x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.m_B);
	for (int i(1); i <= value.m_B.numberOfRows(); ++i) {
		it = value.m_B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r[i].leftBound() << ',' << value.m_r[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, 
					  const capd::dynset::C1PpedSet<capd::IMatrix>& value)
{
	out << "pped[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.m_x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.m_x[i].leftBound() << ',' << value.m_x[i].rightBound() << '}';
		out << value.m_x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.m_B);
	for (int i(1); i <= value.m_B.numberOfRows(); ++i) {
		it = value.m_B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r[i].leftBound() << ',' << value.m_r[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]" << std::endl;
	out.flush();
}

template<>
inline void printPped(std::ostream& out, 
				      const capd::dynset::C1Pped2Set<capd::IMatrix>& value)
{
	out << "pped2[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.m_x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.m_x[i].leftBound() << ',' << value.m_x[i].rightBound() << '}';
		out << value.m_x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.m_B);
	for (int i(1); i <= value.m_B.numberOfRows(); ++i) {
		it = value.m_B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r[i].leftBound() << ',' << value.m_r[i].rightBound() << '}';
	}
	out << std::endl << "}," << std::endl;

	// C
	out << "{ " << std::endl;
	it = value.m_C;
	for (int i(1); i <= value.m_C.numberOfRows(); ++i) {
		it = value.m_C.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_C.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r0
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r0.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r0[i].leftBound() << ',' << value.m_r0[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]" << std::endl;
	out.flush();
}


/// dumpPipe

template<typename Piped>
inline void printPipe(std::ostream& out, const interval& time, const Piped& value)
//inline void printPipe(std::ostream& out,
//					  const interval& time, const capd::C0Rect2Set& value)
{
	out << "{ " << std::endl;

	// time
	printInterval(out, time);

	out << "," << std::endl;

	// each axis
	printPped(out, value);

	out << "}" << std::endl;
	out.flush();
}

} // the end of the namespace capd

#endif // _CAPD_UTIL_H_
