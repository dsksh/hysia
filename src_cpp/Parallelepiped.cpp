#include "util.h"

#include "Context.h"
#include "Parallelepiped.h"

namespace capd{ 

using namespace capd;

std::ostream& operator<<(std::ostream& os, const Parallelepiped& piped) {
	piped.print(os);
	return os;
}

interval norm(const IMatrix& M) {
	return capd::matrixAlgorithms::maxEigenValueOfSymMatrix(M);
}

DMatrix characteristic(const DMatrix& jA) {
	DMatrix B(jA);

	// TODO
	for (int i(0); i < B.numberOfColumns(); ++i) {
		B.column(i).normalize();
	}

	DMatrix B_inv( capd::matrixAlgorithms::inverseMatrix(B) );
	//DMatrix B_inv( capd::matrixAlgorithms::gaussInverseMatrix(B) );

	if (norm(B)*norm(B_inv) > g_context->QrThres) {
		capd::matrixAlgorithms::QR_decompose(B, B, B_inv);
//std::cout << "Q: " << B << std::endl;
//std::cout << "R: " << B_inv << std::endl;
	}
		//capd::matrixAlgorithms::orthonormalize(B);

	return B;
}

Parallelepiped map_parallelepiped(const Parallelepiped& piped,
								  const IMatrix& J, const IVector& y) {
	int dim(y.size());

	DMatrix B(dim,dim);
	IMatrix JA(dim,dim);
	const_MatrixIterator<capd::IMatrix> it1(J);
	const_MatrixIterator<capd::IMatrix> it2(piped.A());
	for (int i(0); i < dim; ++i) {
		for (int j(0); j < dim; ++j) {
			it1 = J.beginOfRow(i+1);
			it2 = piped.A().beginOfColumn(j+1);
			B (i+1,j+1) = 0;
			JA(i+1,j+1) = 0;
			for (int k(0); k < dim; ++k) {
				B (i+1,j+1) += (*it1).mid().leftBound() * (*it2).leftBound();
//std::cout << (*it1).mid() << " * " << (*it2) << std::endl;

				JA(i+1,j+1) += (*it1) * (*it2);
				it1.moveToNextColumn();
				it2.moveToNextRow();
			}
		}
	}

//std::cout << "jA: " << B << std::endl;

#ifndef HSS_BOX_BASED
	B = characteristic(B);
#else
	B = DMatrix::Identity(dim);
#endif

//std::cout << "B: " << B << std::endl;

	IMatrix IB;
	for (int i(0); i < dim; ++i)
		for (int j(0); j < dim; ++j)
			IB(i+1,j+1) = B(i+1,j+1);

	IMatrix B_inv( capd::matrixAlgorithms::inverseMatrix(IB) );

//std::cout << "B_inv: " << B_inv << std::endl;

	IVector v( B_inv*JA * piped.u() + B_inv*(y - capd::vectalg::midVector(y)) );
//std::cout << "v: " << v << std::endl;

	return Parallelepiped(IB, v, capd::vectalg::midVector(y));
}

} // the end of the namespace capd
