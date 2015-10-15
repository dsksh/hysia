#include "Context.h"
#include "Parallelepiped.h"

#include "util.h"

namespace capd{ 

using namespace capd;

std::ostream& operator<<(std::ostream& os, const Parallelepiped& piped) {
	piped.print(os);
	return os;
}

interval norm(const IMatrix& M) {
	//return capd::matrixAlgorithms::maxEigenValueOfSymMatrix(M);
	static IEuclNorm norm;
	return norm(M);
}

DMatrix characteristic(const DMatrix& jA) {
	DMatrix B(jA);

	TRY {
		// TODO
		for (int i(0); i < B.numberOfColumns(); ++i) {
    		//if (!isSingular(B.column(i).euclNorm()))
      		//	THROW("Failed to decompose B");

			// should compute: Bcol(i) / norm(Bcol(i))
			B.column(i).normalize();
		}

		//DMatrix B_inv( capd::matrixAlgorithms::inverseMatrix(B) );
		//DMatrix B_inv( capd::matrixAlgorithms::gaussInverseMatrix(B) );
		DMatrix B_inv;
		try { 
			B_inv = capd::matrixAlgorithms::inverseMatrix(B);
		} catch (std::runtime_error &e) {
		    std::cout << "runtime_error from CAPD: " << e.what () << std::endl;
		
			// TODO: it won't recover
			B = DMatrix::Identity(B.numberOfColumns());
		}

		if ( g_params->cm_thres == 1 ||
			(g_params->cm_thres >  1 && norm(B)*norm(B_inv) > g_params->cm_thres) ) {

			capd::matrixAlgorithms::QR_decompose(B, B, B_inv);
//std::cout << "Q: " << B << std::endl;
//std::cout << "R: " << B_inv << std::endl;

			//capd::matrixAlgorithms::orthonormalize(B);
		}
	
		return B;
	
	}
	//catch (std::runtime_error &e) {
	CATCH {
	    //std::cerr << "runtime_error from CAPD: " << eh_ex.what() << std::endl;

		// TODO: it won't recover
		return DMatrix::Identity(B.numberOfColumns());
	}
}

Parallelepiped map_parallelepiped(const Parallelepiped& piped,
								  const IMatrix& J, const IVector& y) {
	int dim(y.size());

//std::cout << "J: " << J << std::endl;

	// compute B and JA
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
				JA(i+1,j+1) += (*it1) * (*it2);
				it1.moveToNextColumn();
				it2.moveToNextRow();
			}
		}
	}

//std::cout << "jA: " << B << std::endl;

	if (g_params->cm_thres != 0)
		B = characteristic(B);
	else
		B = DMatrix::Identity(dim);

	IMatrix IB;
	for (int i(0); i < dim; ++i)
		for (int j(0); j < dim; ++j)
			IB(i+1,j+1) = B(i+1,j+1);

	IMatrix B_inv;
	try { 
		B_inv = capd::matrixAlgorithms::inverseMatrix(IB);
	} catch (std::runtime_error &e) {
	    std::cout << "runtime_error from CAPD: " << e.what () << std::endl;
	
		// TODO: it won't recover
		B_inv = IMatrix::Identity(B.numberOfColumns());
	}

//std::cout << "B_inv: " << B_inv << std::endl;

	IVector v( B_inv*JA * piped.u() + B_inv*(y - capd::vectalg::midVector(y)) );

	return Parallelepiped(IB, v, capd::vectalg::midVector(y));
}

} // the end of the namespace capd
