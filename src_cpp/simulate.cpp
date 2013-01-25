#include "Context.h"
#include "nodebuilder.h"
#include "Parallelepiped.h"
#include "util.h"

#include "simulate.h"

using namespace std;
using namespace capd;

void simInitialize()
{
	cout.precision(17);
	
	g_context->initPped();
}


IVector simulate(IMap& der, const IVector& x, const interval& time)
{
	IVector result;
	try{
		ITaylor solver(der, /**/20, /**/0.1);
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

IVector simulate_deriv(IMap& der, const IVector& x, const interval& time,
					   IMatrix& dx_phi, IVector& dt_phi)
{
	IVector result;
	try{
		ITaylor solver(der, /**/20, /**/0.1);
		ITimeMap timeMap(solver);

		// define a doubleton representation of the interval vector x
		// and the derivative wrt initial condition.
		IEuclLNorm n;
		C1Rect2Set s(x, n);

		result = timeMap(time, s, dx_phi);

		// TODO
		//dx_phi = der[result];
		dt_phi = der(result);
	} 
	catch(std::exception& e) {
		std::cerr << "\n\nException caught!\n" << e.what() << std::endl;
	}

	return result;
}


void simulateJump()
{
	int dim(g_context->dim);
	DerMap& der = g_context->der;
	AuxMap& jump = g_context->jump;

	Parallelepiped& pped = g_context->pped;
	const interval& time = g_context->time;
	const interval& time_mid = g_context->time_mid;
	double time_l = g_context->time_l;

	const IVector& x      = g_context->x;
	const IVector& x_mid  = g_context->x_mid;
	const IVector& x_left = g_context->x_left;
	const IMatrix& dx_phi = g_context->dx_phi;
	const IVector& dt_phi = g_context->dt_phi;
	const IVector& dh     = g_context->dh;

	// omega_mid
	IVector delta_y_mid( jump(x_mid) );
	IVector omega_mid( simulate(der, delta_y_mid, 
							time.right()-time_mid) );

#ifdef HSS_DEBUG
std::cout << "omega_mid: " << omega_mid << std::endl;
#endif

	// D_omega

//std::cout << "Dx_phi: " << Dx_phi << std::endl;
//std::cout << "Dh: " << Dh << std::endl;

	IVector dt_num(dim);
	const_MatrixIterator<capd::IMatrix> it(dx_phi);
	for (int i(0); i < dim; ++i) {
		it = dx_phi.beginOfColumn(i+1);
		for (int j(0); j < dim; ++j) {
			dt_num[i] += dh[j]*(*it);
			it.moveToNextRow();
		}
	}

//std::cout << "Dt_num: " << Dt_num << std::endl;

	interval dh_dt_phi(0);
	for (int i(0); i < dim; ++i) {
		dh_dt_phi += dh[i]*dt_phi[i];
	}

	const IVector dt( -dt_num / dh_dt_phi );

#ifdef HSS_DEBUG
std::cout << "Dt: " << dt_phi << std::endl;
#endif


	IMatrix dt_phi_dt(dim,dim);
	//Dt_phi_Dt = Dt_phi * Dt_phi;
	for (int i(0); i < dim; ++i) {
		for (int j(0); j < dim; ++j) {
			dt_phi_dt(i+1,j+1) = dt_phi[i] * dt[j];
		}
	}

	const IMatrix d_delta( jump[x] );
	const IMatrix delta( d_delta * (dx_phi+dt_phi_dt) );

	const IVector delta_y( jump(x) );
	IMatrix dx_psi(dim,dim);
	IVector dt_psi(dim);
	simulate_deriv(der, delta_y, time-time_l, dx_psi, dt_psi);

	IMatrix dx_psi_delta( dx_psi * delta );

	IMatrix dt_psi_dt(dim,dim);
	for (int i(0); i < dim; ++i) {
		for (int j(0); j < dim; ++j) {
			dt_psi_dt(i+1,j+1) = dt_psi[i]*dt[j];
		}
	}

	const IMatrix d_omega( dx_psi_delta - dt_psi_dt );
#ifdef HSS_DEBUG
std::cout << "D_omega: " << d_omega << std::endl;
#endif

	pped = map_parallelepiped(pped, d_omega, omega_mid);
}
