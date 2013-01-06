#include <memory>
//#include "boost/shared_ptr.hpp"

//#include "capd/dynset/C1PpedSet.h"
//#include "capd/dynset/C1Pped2Set.h"
//#include "capd/matrixAlgorithms/matrixAlgorithmsLib.h"

#include "nodebuilder.h"
#include "parallelotope.h"
#include "util.h"

#include "firstzero.h"

using namespace std;
using namespace capd;
//using namespace capd::map;

typedef auto_ptr<Parallelepiped> PpedPtr;
typedef auto_ptr<double> DblPtr;
typedef auto_ptr<interval> IntPtr;
typedef auto_ptr<IVector> IVecPtr;
typedef auto_ptr<IMatrix> IMatPtr;

//template<typename FunH, typename FunG>
//bool first_zero(FunH guard_h, FunG guard_g, IMap& M, CapdPped& P, 
//				const double g_time_l, interval& g_time,
//				IVector& X, IVector& X_left, IMatrix& Dx, IVector& Dt, IVector& Dh)

PpedPtr P;
IntPtr g_time;
DblPtr g_time_l;
IVecPtr X;
IVecPtr X_left;
IMatPtr Dx;
IMatPtr Dx_prev;
IVecPtr Dt;
IVecPtr Dh;

void simInitialize()
{
	IMatrix A(IMatrix::Identity(g_dim));
    IVector x(midVector(*g_ivec));
    IVector u(*g_ivec - x);
	P = PpedPtr(new Parallelepiped(A, u, x));

	g_time = IntPtr(new interval());
	g_time_l = DblPtr(new double(0.));
}

int firstZero()
{
	X = IVecPtr(new IVector(g_dim));
	X_left = IVecPtr(new IVector(g_dim));
	Dx = IMatPtr(new IMatrix(g_dim,g_dim));
	Dx_prev = IMatPtr(new IMatrix(g_dim,g_dim));
	Dt = IVecPtr(new IVector(g_dim));
	Dh = IVecPtr(new IVector(g_dim));

	try{

	// the solver:
	ITaylor solver(*g_der, /**/20, /**/0.01);
	ITimeMap timeMap(solver);

	// the initial value:
	///C0Rect2Set s(*g_ivec);
	CapdPped p(P->toCapdPped());

	cout << '{' << endl;
	//dumpPipe1(cout, 0, s);

	// Here we start to integrate.
	// The time of integration:
	double T(/*t_end*/10);
	timeMap.stopAfterStep(true);
	interval prevTime(0.);

    interval time_procd(*g_time_l);

	//try{
	do {
		timeMap(MaxTime, p);
		*g_time = interval(0,1)*solver.getStep();
		interval time_step(*g_time);
#ifdef HSS_DEBUG
std::cout << std::endl << "step made: " << time_procd + *g_time << std::endl;
#endif
		const ITaylor::CurveType& curve = solver.getCurve();

		// reduce the lower bound

		// TODO: inside/outside of the loop?
		//IVector dv(curve.derivative()(g_time)); // TODO
		IVector dv( (*g_der)(curve(*g_time)) );
		interval dh( (*g_grd).der()(1)*dv );
		//interval dg( (*g_grd)[ curve(g_time) ]*dv);
#ifdef HSS_DEBUG
std::cout << "x:  " << curve(*g_time) << std::endl;
std::cout << "dh: " << dh << std::endl;
//std::cout << "dh: " << (*g_grd).der()(1) << std::endl;
//std::cout << "g:  " << guard_g( curve(g_time) ) << std::endl;
//std::cout << "dg: " << dg << std::endl;
#endif

		interval time_bak(*g_time);
		interval time_old;
		bool is_empty(false);
		//int i(0);
		do {
			//if (i++ > 5) break;

			time_old = *g_time;

			interval offset(g_time->left());
			IVector v_l( curve(offset) );
			IVector dv( (*g_der)(curve(*g_time)) );
			interval h( (*g_grd)()(1) );
			//interval g(guard_g(v_l) + interval(0,INFINITY));

#ifdef HSS_DEBUG
std::cout << std::endl << "contracting lb:\t" << *g_time << std::endl;
std::cout << "h:\t\t" << h << std::endl;
//std::cout << "g:\t\t" << g << std::endl;
#endif

			*g_time -= offset;
			interval *gamma_l(g_time.get());
			interval *gamma_u(NULL);
			extDiv(dh, -h, &gamma_l, &gamma_u);

			if (gamma_l == NULL) {
				is_empty = true;
				break;
			}
			else if (gamma_u == NULL) {
				//g_time = *gamma_l;
			}
			else {
				//g_time = *gamma_u;
				delete gamma_u;
			}

//			extDiv(dg, -g, gamma_l, gamma_u);
//
//			if (gamma_l == NULL) {
//				is_empty = true;
//				break;
//			}
//			else if (gamma_u == NULL) {
//				g_time = *gamma_l;
//			}
//			else {
//				g_time = *gamma_u;
//				delete gamma_u;
//			}

			*g_time += offset;
			intersection(time_bak, *g_time, *g_time);
#ifdef HSS_DEBUG
std::cout << "contracted:\t" << time_procd + *g_time << std::endl;
#endif

		} while (!is_empty && 
				 //time_old != *g_time);
				 hausdorff(time_old, *g_time) >= HSS_EPSILON);

		if (is_empty) {
			time_procd = *g_time_l + timeMap.getCurrentTime();
			*Dx_prev = IMatrix(p);
			continue;
		}

//std::cout << "contracted:\t" << *g_time+time_procd << std::endl;
//std::cout << "contracted:\t" << *g_time << std::endl;

		// certification of the result with inflation process

		interval time_bakbak(time_bak);
		time_bak = *g_time;
		time_old = UNIVERSE;
		//time_old = *g_time;
		*g_time = g_time->left();
		double d(INFINITY), d_old(INFINITY);
		bool proved(false);

		while (hausdorff(time_old, *g_time) <= HSS_DELTA*d_old) {

#ifdef HSS_DEBUG
std::cout << std::endl << "certifying:\t" << *g_time+time_procd << std::endl;
#endif
			interval time_tmp;
			intersection(time_bakbak, *g_time, time_tmp);

			//IVector dv(curve.derivative()(time_tmp));
			IVector dv( (*g_der)(curve(time_tmp)) );
			interval dh( (*g_grd).der()(1)*dv );
#ifdef HSS_DEBUG
std::cout << "dh: " << dh << " at " << time_tmp << std::endl;
#endif
			if ( dh.contains((capd::TypeTraits<interval>::zero())) ) {
				// TODO
				throw "zero in the derivative";
				//std::cerr << "zero in the derivative" << std::endl;
				//proved = false;
				//break;
			}

			(*g_der)(curve(g_time->left()));
			interval contracted(g_time->left() - (*g_grd)()(1) / dh);
#ifdef HSS_DEBUG
std::cout << "contracted:\t" << contracted << std::endl;
#endif

			if (g_time->containsInInterior(contracted)) {
#ifdef HSS_DEBUG
std::cout << "proved" << std::endl;
#endif

				//intersection(time_bak, contracted, *g_time);

				proved = true;
				break;
			}

			//if (!intersection(time_bak, contracted, contracted))
			//	throw "INewton does not converge";

			d_old = d;
			d = hausdorff(time_old, contracted);
			time_old = contracted;

			//g_time.setRightBound(g_time.leftBound() + 
			//				   HSS_TAU*(contracted.rightBound() - g_time.leftBound()));
			*g_time = g_time->mid() + HSS_TAU*(contracted - g_time->mid());
#ifdef HSS_DEBUG
std::cout << "inflated:\t" << *g_time << std::endl;
#endif
		}

		if (!proved) {
			intersection(time_bak, *g_time, *g_time);
			return false;

			// TODO
			//time_procd = *g_time_l + timeMap.getCurrentTime();
			//Dx_prev = IMatrix(P);
			//continue;
		}

		/*if (g_time.right() == time_bak.right()) {
			X  = curve(*g_time);
			Dx = curve[*g_time]*Dx_prev;
			Dt = curve.derivative()(*g_time);
			Dh = guard_h[X];
			*g_time += time_procd;
			return true;
		}*/

		// TODO
		intersection(time_bak, *g_time, *g_time);


		// reduce right bound
/* temporary commented out */
		//dv = curve.derivative()(*g_time);
		dv = (*g_der)(curve(*g_time));
		dh = (*g_grd).der()(1)*dv;

		do {
#ifdef HSS_DEBUG
//std::cout << std::endl << "contracting rb: " << *g_time << std::endl;
#endif

			time_old = *g_time;

			interval offset(g_time->right());
			(*g_der)( curve(offset) );
			interval h( (*g_grd)()(1) );
			*g_time -= offset;
			interval *gamma_l(g_time.get());
			interval *gamma_u(NULL);
			extDiv(dh, -h, &gamma_l, &gamma_u);

			if (gamma_l == NULL) {
//std::cout << "gamma: null" << std::endl;
				throw "gamma is empty";
			}
			else if (gamma_u == NULL) {
//std::cout << "gamma: " << *gamma_l << std::endl;
				*g_time = *gamma_l;
			}
			else {
//std::cout << "gamma_l: " << *gamma_l << ", gamma_u: " << *gamma_u << std::endl;
				*g_time = *gamma_u;
				delete gamma_u;
			}
			*g_time += offset;
			intersection(time_old, *g_time, *g_time);
//std::cout << "refined:\t" << time_procd + *g_time << std::endl;

		} while (time_old != *g_time);

#ifdef HSS_DEBUG
std::cout << "refined:\t" << time_procd + *g_time << std::endl;
#endif

		*X = curve(*g_time);

		*X_left = curve(g_time->left());
#ifndef HSS_SKIP_PPED_T_INF
		*Dx = curve[*g_time]*(*Dx_prev);
#else
		*Dx = curve[g_time->left()]*(*Dx_prev);
#endif
		*Dt = curve.derivative()(*g_time);
		*Dt = (*g_der)(*X);

		*Dh = (*g_grd).der()(1);
		*g_time += time_procd;
		return true;

/*		//IVector v = timeMap(T,s);
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
			dumpPipe1(cout, prevTime+subsetOfDomain, v);
		}
		prevTime = timeMap.getCurrentTime();
*/
	} while(!timeMap.completed());

//	dumpPipe1(cout, timeMap.getCurrentTime(), s, false);
//	cout << "}" << endl;

	} catch(exception& e)
	{
		cout << "\n\nException caught!\n" << e.what() << endl << endl;
	}

}
