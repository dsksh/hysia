#include "nodebuilder.h"
#include "parallelotope.h"
#include "util.h"

#include "firstzero.h"

using namespace std;
using namespace capd;
//using namespace capd::map;

PpedPtr P;
IntPtr g_time;
IntPtr g_time_mid;
DblPtr g_time_l;
IVecPtr X;
IVecPtr X_mid;
IVecPtr X_left;
IMatPtr Dx;
IVecPtr Dt;
IVecPtr Dh;

void simInitialize()
{
	cout.precision(17);
	
	IMatrix A(IMatrix::Identity(g_dim));
    IVector x(midVector(*g_ivec));
    IVector u(*g_ivec - x);
	P = PpedPtr(new Parallelepiped(A, u, x));

	g_time = IntPtr(new interval());
	g_time_mid = IntPtr(new interval());
	g_time_l = DblPtr(new double(0.));

	//
	X = IVecPtr(new IVector(g_dim));
	X_mid = IVecPtr(new IVector(g_dim));
	X_left = IVecPtr(new IVector(g_dim));
	Dx = IMatPtr(new IMatrix(g_dim,g_dim));
	Dt = IVecPtr(new IVector(g_dim));
	Dh = IVecPtr(new IVector(g_dim));
}


IVector simulate(IMap& M, const IVector& x, const interval& time)
{
	IVector result;
	try{
		// the solver, is uses high order enclosure method to verify the existence 
		// of the solution. The order is set to 20.
		// The time step (when step control is turned off) will be 0.1.
		ITaylor solver(M, 20, 0.1);

		ITimeMap timeMap(solver);

		// define a doubleton representation of the interval vector x
		C0Rect2Set s(x);

		result = timeMap(time, s);
	} 
	catch(std::exception& e) {
		std::cerr << "\n\nException caught!\n" << e.what() << std::endl;
	}

	return result;
}

IVector simulate_deriv(IMap& M, const IVector& x, const interval& time,
					   IMatrix& Dx, IVector& Dt)
{
	IVector result;
	try{
		// the solver, is uses high order enclosure method to verify the existence 
		// of the solution. The order is set to 20.
		// The time step (when step control is turned off) will be 0.1.
		ITaylor solver(M, 20, 0.1);

		ITimeMap timeMap(solver);

		// define a doubleton representation of the interval vector x
		// and the derivative wrt initial condition.
		IEuclLNorm N;
		C1Rect2Set s(x, N);

		result = timeMap(time, s, Dx);

		// TODO
		//Dx = M[result];
		Dt = M(result);
	} 
	catch(std::exception& e) {
		std::cerr << "\n\nException caught!\n" << e.what() << std::endl;
	}

	return result;
}


void simulateJump()
{
/*
	int dim(X->size());

	// omega_mid

	//IVector y_mid( simulate(M, midVector(X), time.right()-time.left()) );
	IVector delta_y_mid( (*g_jump)(*X_mid) );
	//IVector omega_mid( simulate(M, delta_y_mid, time.mid()-time.left()) );
	IVector omega_mid( simulate(M, delta_y_mid, time.right()-time_mid) );

#ifdef HSS_DEBUG
std::cout << "omega_mid: " << omega_mid << std::endl;
#endif

	// D_omega

//std::cout << "Dx_phi: " << Dx_phi << std::endl;
//std::cout << "Dh: " << Dh << std::endl;

	IVector Dt_num(dim);
	const_MatrixIterator<capd::IMatrix> it(Dx_phi);
	for (int i(0); i < dim; ++i) {
		it = Dx_phi.beginOfColumn(i+1);
		for (int j(0); j < dim; ++j) {
			Dt_num[i] += Dh[j]*(*it);
			it.moveToNextRow();
		}
	}

//std::cout << "Dt_num: " << Dt_num << std::endl;

	interval Dh_Dt_phi(0);
	for (int i(0); i < dim; ++i) {
		Dh_Dt_phi += Dh[i]*Dt_phi[i];
	}

	IVector Dt( - Dt_num / Dh_Dt_phi );

#ifdef HSS_DEBUG
std::cout << "Dt: " << Dt << std::endl;
#endif


	IMatrix Dt_phi_Dt(dim,dim);
	//Dt_phi_Dt = Dt_phi * Dt;
	for (int i(0); i < dim; ++i) {
		for (int j(0); j < dim; ++j) {
			Dt_phi_Dt(i+1,j+1) = Dt_phi[i] * Dt[j];
		}
	}

	IMatrix D_delta( delta[X] );
	IMatrix Delta( D_delta * (Dx_phi+Dt_phi_Dt) );

	IVector delta_y( delta(X) );
	IMatrix Dx_psi(dim,dim);
	IVector Dt_psi(dim);
	simulate_deriv(M, delta_y, time-time.left(), Dx_psi, Dt_psi);

	IMatrix Dx_psi_Delta( Dx_psi * Delta );

	IMatrix Dt_psi_Dt(dim,dim);
	for (int i(0); i < dim; ++i) {
		for (int j(0); j < dim; ++j) {
			Dt_psi_Dt(i+1,j+1) = Dt_psi[i]*Dt[j];
		}
	}

	IMatrix D_omega( Dx_psi_Delta - Dt_psi_Dt );
#ifdef HSS_DEBUG
std::cout << "D_omega: " << D_omega << std::endl;
#endif

	P = map_parallelepiped(P, D_omega, omega_mid);
*/
}
