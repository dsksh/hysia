#include <memory>
#include <iostream>
#include <fstream>

#include "Context.h"
#include "Parallelepiped.h"
#include "util.h"

#include "simulate.h"

using namespace std;
using namespace capd;

CtxPtr g_context;

typedef auto_ptr<ofstream> fstreamPtr;
fstreamPtr g_fstream;

void simInitialize()
{
	std::cout.precision(17);
	std::cout.setf(ios::fixed,ios::floatfield);
	
	g_fstream = fstreamPtr(new ofstream("pped.dat"));
	g_fstream->precision(17);
	g_fstream->setf(ios::fixed,ios::floatfield);

	//g_context = CtxPtr(new Context(*g_model, cnull, *g_fstream));
	g_context = CtxPtr(new Context(*g_model, cout, *g_fstream));

	g_context->fout << '{' << endl;
}

void simDispose()
{
	g_context->fout << '}' << endl;
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
		std::cerr << "\n\nException caught!\n" << e.what() << endl;
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
		std::cerr << "\n\nException caught!\n" << e.what() << endl;
	}

	return result;
}


void simulateJump(const char *lid, const char *dst, const cInterval time0)
{
	int dim(g_model->dim);
	DerMap& der = g_model->locs[lid]->der;
	AuxMap& jump = g_model->locs[lid]->edges[dst]->jump;

	Parallelepiped& pped = g_context->pped;
//g_context->time = interval(time0.l, time0.u);
	const interval& time = g_context->time;
	const interval& time_mid = g_context->time_mid;
	double time_l = g_context->time_l;
//const double time_l(g_context->time.rightBound());

	// FIXME
	const IVector& x      = g_context->x;
	const IVector& x_mid  = g_context->x_mid;
	const IMatrix& dx_phi = g_context->dx_phi;
	const IVector& dt_phi = g_context->dt_phi;
	const IVector& dh     = g_context->dh;

	// omega_mid
	IVector delta_y_mid( jump(x_mid) );
	IVector omega_mid( simulate(der, delta_y_mid, 
							time.right()-time_mid) );

g_context->cout << "omega_mid: " << omega_mid << endl;

	// D_omega
	IVector dt_num(dim);
	const_MatrixIterator<capd::IMatrix> it(dx_phi);
	for (int i(0); i < dim; ++i) {
		it = dx_phi.beginOfColumn(i+1);
		for (int j(0); j < dim; ++j) {
			dt_num[i] += dh[j]*(*it);
			it.moveToNextRow();
		}
	}

	interval dh_dt_phi(0);
	for (int i(0); i < dim; ++i) {
		dh_dt_phi += dh[i]*dt_phi[i];
	}

	const IVector dt( -dt_num / dh_dt_phi );

g_context->cout << "Dt: " << dt_phi << endl;


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
g_context->cout << "D_omega: " << d_omega << endl;

	pped = map_parallelepiped(pped, d_omega, omega_mid);
}

void integrate(const char *lid, 
			   const float t_end, const float order, const float h_min, const float h_max)
{
 	try{
 
 	// The solver:
 	ITaylor solver(g_model->locs[lid]->der, order, h_min);
 	ITimeMap timeMap(solver);
 
 	// The initial value:
 	C0Rect2Set s(g_model->x_init);
 
 	cout << '{' << endl;
 	printPipe(cout, 0, s);
 
 	// Here we start to integrate.
 	// The time of integration:
 	double T(t_end);
 	timeMap.stopAfterStep(true);
 	interval prevTime(0.);
 
 	//try{
 	do 
 	{
 		//IVector v = timeMap(T,s);
 		timeMap.moveSet(T, s);
 
 		interval stepMade(solver.getStep());
 		const ITaylor::CurveType& curve = solver.getCurve();
 
 		int grid(stepMade.rightBound()/h_max + 0.9999999999);
 		if (grid==0) grid = 1;
 //cout << stepMade.rightBound()/h_max << endl;
 		for(int i=0;i<grid;++i)
 		{
 			interval subsetOfDomain = interval(i,i+1)*stepMade/grid;
 
 			IVector v = curve(subsetOfDomain);
 			printPipe(cout, prevTime+subsetOfDomain, v);
 		}
 		prevTime = timeMap.getCurrentTime();
 
 	} while(!timeMap.completed());
 
 	printPipe(cout, timeMap.getCurrentTime(), s);
 	cout << "}" << endl;
 
 	} catch(exception& e)
 	{
 		cout << "\n\nException caught!\n" << e.what() << endl << endl;
 	}
 }
