#ifndef _CAPD_PPED_H_
#define _CAPD_PPED_H_

#include "capd/dynset/C0Rect2Set.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"
#include "capd/matrixAlgorithms/matrixAlgorithmsLib.h"

//#include "util.h"

namespace capd{ 

//typedef capd::dynset::C1PpedSet<IMatrix> CapdPped;
//typedef capd::dynset::C1Pped2Set<IMatrix> CapdPped;
typedef capd::dynset::C1RectSet<IMatrix> CapdPped;
//typedef capd::dynset::C1Rect2Set<IMatrix> CapdPped;
//typedef capd::dynset::C1Rect2RSet<IMatrix> CapdPped;
//typedef capd::dynset::C0Rect2Set<IMatrix> CapdBox;

class Parallelepiped
{
public:
	Parallelepiped()
	{}

	Parallelepiped(const capd::IMatrix& A, const capd::IVector& u, const capd::IVector& x)
		: m_A(A), m_u(u), m_x(x)
	{}

	Parallelepiped(const Parallelepiped& rhs)
		: m_A(rhs.m_A), m_u(rhs.m_u), m_x(rhs.m_x)
	{}

	const capd::IMatrix& A() const {
		return m_A;
	}

	const capd::IVector& u() const {
		return m_u;
	}

	const capd::IVector& x() const {
		return m_x;
	}

	CapdPped toCapdPped() {
		IEuclNorm r;
		return CapdPped(m_x, m_A, m_u, r);
	}

	void print(std::ostream& os) const {
		os << "x: " << m_x << std::endl;
		os << "A: " << m_A << std::endl;
		os << "u: " << m_u << std::endl;
	}

	capd::IVector hull() const {
		return m_x + m_A*m_u;
	}

private:
	capd::IMatrix m_A;
	capd::IVector m_u;
	capd::IVector m_x;
};

std::ostream& operator<<(std::ostream& os, const Parallelepiped& piped);
interval norm(const capd::IMatrix& M);
capd::DMatrix characteristic(const capd::DMatrix& jA);
Parallelepiped map_parallelepiped(const Parallelepiped& piped,
								  const capd::IMatrix& J, const capd::IVector& y);

} // the end of the namespace capd

#endif // _CAPD_PPED_H_
