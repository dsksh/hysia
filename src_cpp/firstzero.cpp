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

inline bool reduceLower(const ITaylor::CurveType& curve,
						const interval& time_init, const interval& time_procd,
						interval& time)
{
	interval time_old;
	//int i(0);
	do {
		//if (i++ > 5) break;

		time_old = time;

		// current state
		const IVector dx( (*g_der)(curve(time)) );
		//const interval h ( (*g_grd_h)()(1) );
		const interval dh( (*g_grd_h).der()(1)*dx );
		//const interval g ( (*g_grd_g)()(1) + interval(0,INFINITY) );
		const interval dg( (*g_grd_g).der()(1)*dx );

#ifdef HSS_DEBUG
std::cout << std::endl << "contracting lb:\t" << time << std::endl;
//std::cout << "h:\t\t" << h << std::endl;
//std::cout << "g:\t\t" << g << std::endl;
#endif

		// state at the left bound.
		const interval offset(time.left());
		(*g_der)(curve(offset));
		const interval h( (*g_grd_h)()(1) );
		const interval g( (*g_grd_g)()(1) + interval(0,INFINITY) );
		time -= offset;

		interval *gamma_l(&time);
		interval *gamma_u(NULL);
		extDiv(dh, -h, &gamma_l, &gamma_u);

		if (gamma_l == NULL) {
			return false;
		}
		else if (gamma_u == NULL) {
			//time = *gamma_l;
		}
		else {
			//time = *gamma_u;
			delete gamma_u;
		}

		extDiv(dg, -g, &gamma_l, &gamma_u);

		if (gamma_l == NULL) {
			return false;
		}
		else if (gamma_u == NULL) {
			time = *gamma_l;
		}
		else {
			time = *gamma_u;
			delete gamma_u;
		}

		time += offset;
		intersection(time_init, time, time);
#ifdef HSS_DEBUG
std::cout << "contracted lb:\t" << time_procd + time << std::endl;
#endif

	} while (//time_old != time);
			 hausdorff(time_old, time) >= HSS_EPSILON);

	return true;
}

inline bool verify(const ITaylor::CurveType& curve,
				   const interval& time_init, const interval& time_procd,
				   interval& time)
{
	//const interval time_bak(time);
	interval time_old(UNIVERSE);
	time = time.left();
	double d(INFINITY), d_old(INFINITY);

	do {

#ifdef HSS_DEBUG
std::cout << std::endl << "certifying:\t" << time+time_procd << std::endl;
#endif
		// current state
		interval time_tmp;
		intersection(time_init, time, time_tmp);
		const IVector  dx( (*g_der)(curve(time_tmp)) );
		const interval dh( (*g_grd_h).der()(1)*dx );
#ifdef HSS_DEBUG
std::cout << "dh: " << dh << " at " << time_tmp << std::endl;
#endif
		if ( dh.contains((capd::TypeTraits<interval>::zero())) ) {
			// TODO
			throw "zero in the derivative";
			//std::cerr << "zero in the derivative" << std::endl;
			//break;
		}

		// interval Newton
		(*g_der)(curve(time.left()));
		const interval contracted(time.left() - (*g_grd_h)()(1) / dh);
#ifdef HSS_DEBUG
std::cout << "contracted:\t" << contracted+time_procd << std::endl;
#endif
		if (time.containsInInterior(contracted)) {
#ifdef HSS_DEBUG
std::cout << "proved" << std::endl;
#endif
			return true;
		}

		// inflation
		d_old = d;
		d = hausdorff(time_old, contracted);
		time_old = contracted;

		//time = time.mid() + HSS_TAU*(contracted - time.mid());
		time = contracted.mid() + HSS_TAU*(contracted - contracted.mid());
#ifdef HSS_DEBUG
std::cout << "inflated:\t" << time+time_procd << std::endl;
#endif

	} while (hausdorff(time_old, time) <= HSS_DELTA*d_old);

	return false;
}

inline bool reduceUpper(const ITaylor::CurveType& curve,
					    const interval& time_init, const interval& time_procd,
					    interval& time)
{
	interval time_old;

	do {
#ifdef HSS_DEBUG
//std::cout << std::endl << "contracting rb: " << *g_time << std::endl;
#endif

		time_old = time;

		// current state.
		const IVector  dx = (*g_der)(curve(time));
		const interval dh = (*g_grd_h).der()(1)*dx;

		// state at the right bound.
		const interval offset(time.right());
		(*g_der)( curve(offset) );
		const interval h( (*g_grd_h)()(1) );
		time -= offset;

		interval *gamma_l(g_time.get());
		interval *gamma_u(NULL);
		extDiv(dh, -h, &gamma_l, &gamma_u);

		if (gamma_l == NULL) {
			//throw "gamma is empty";
			return false;
		}
		else if (gamma_u == NULL) {
			time = *gamma_l;
		}
		else {
			time = *gamma_u;
			delete gamma_u;
		}
		time += offset;
		intersection(time_old, time, time);

	} while (//time_old != time);
			 hausdorff(time_old, time) >= HSS_EPSILON);

	return true;
}


int findFirstZero()
{
	try{

	// the solver:
	ITaylor solver(*g_der, /**/20, /**/0.01);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	///C0Rect2Set s(*g_ivec);
	CapdPped p(P->toCapdPped());

	cout << '{' << endl;
	//dumpPipe1(cout, 0, s);

    interval time_procd(*g_time_l);
	IMatrix dx_prev(g_dim,g_dim);

	while (true) {
		// integrate 1 step.
		timeMap(MaxTime, p);
		if (timeMap.completed())
			return false;

		*g_time = interval(0,1)*solver.getStep();
#ifdef HSS_DEBUG
std::cout << std::endl << "step made: " << time_procd + *g_time << std::endl;
#endif
		const ITaylor::CurveType& curve = solver.getCurve();
		const interval time_init(*g_time);

		// TODO: inside/outside of the loop?
		IVector  dx( (*g_der)(curve(*g_time)) );
#ifdef HSS_DEBUG
std::cout << "x:  " << curve(*g_time) << std::endl;
std::cout << "dx: " << dx << std::endl; 
//std::cout << "h:  " << (*g_grd_h)()(1) << std::endl;
//std::cout << "dh: " << (*g_grd_h).der()(1)*dx << std::endl;
//std::cout << "g:  " << (*g_grd_g)()(1) << std::endl;
//std::cout << "dg: " << (*g_grd_g).der()(1)*dx << std::endl;
#endif

		// reduce the lower bound
		if (reduceLower(curve, time_init, time_procd, *g_time))
			break;

		time_procd = *g_time_l + timeMap.getCurrentTime();
		dx_prev = IMatrix(p);
	}

	const ITaylor::CurveType& curve = solver.getCurve();
	const interval time_init(*g_time);

//std::cout << "contracted:\t" << *g_time+time_procd << std::endl;

	// verification of the result
	if (!verify(curve, time_init, time_procd, *g_time)) {
		throw "verification failed";

		// TODO
		//time_procd = *g_time_l + timeMap.getCurrentTime();
		//dx_prev = IMatrix(P);
		//continue;
	}

	/*if (g_time.right() == time_init.right()) {
		X  = curve(*g_time);
		Dx = curve[*g_time]*dx_prev;
		Dt = curve.derivative()(*g_time);
		Dh = guard_h[X];
		*g_time += time_procd;
		return true;
	}*/

	// TODO
	//intersection(time_init, *g_time, *g_time);

	// reduce the upper bound
	if (!reduceUpper(curve, time_init, time_procd, *g_time))
		throw "failed in reducing the upper bound";

#ifdef HSS_DEBUG
std::cout << "contracted ub:\t" << *g_time + time_procd << std::endl;
#endif

	*X = curve(*g_time);
	*X_left = curve(g_time->left());
#ifndef HSS_SKIP_PPED_T_INF
	*Dx = curve[*g_time]*dx_prev;
#else
	*Dx = curve[g_time->left()]*dx_prev;
#endif
	*Dt = curve.derivative()(*g_time);
	*Dt = (*g_der)(*X);
	*Dh = (*g_grd_h).der()(1);
	*g_time += time_procd;

//	dumpPipe1(cout, timeMap.getCurrentTime(), s, false);
//	cout << "}" << endl;

	} catch(exception& e)
	{
		cout << "exception caught!\n" << e.what() << endl << endl;
		return false;
	}

	return true;
}


int findFirstZeroMid()
{
	try{

	// the solver:
	ITaylor solver(*g_der, /**/20, /**/0.01);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdBox p(P->x());

    interval time_procd(*g_time_l);

	while (true) {
		// integrate 1 step.
		timeMap(MaxTime, p);
		if (timeMap.completed())
			return false;

		*g_time_mid = interval(0,1)*solver.getStep();
#ifdef HSS_DEBUG
std::cout << std::endl << "step made: " << time_procd + *g_time_mid << std::endl;
#endif
		if (intersection(*g_time-time_procd, *g_time_mid, *g_time_mid))
			break;

		time_procd = *g_time_l+timeMap.getCurrentTime();
	}

	const ITaylor::CurveType& curve = solver.getCurve();

	// reduce with the interval Newton
    //interval contracted(*g_time_mid);
    interval time_old;
	do {
    	time_old = *g_time_mid;

		const IVector  dx( (*g_der)(curve(*g_time_mid)) );
		const interval dh( (*g_grd_h).der()(1)*dx );
#ifdef HSS_DEBUG
std::cout << "contracting:\t" << *g_time_mid+time_procd << std::endl;
#endif
		(*g_der)(curve(g_time_mid->mid()));
		*g_time_mid = g_time_mid->mid() - (*g_grd_h)()(1) / dh;
#ifdef HSS_DEBUG
std::cout << "contracted:\t" << *g_time_mid+time_procd << std::endl;
#endif

		if (!intersection(*g_time-time_procd, *g_time_mid, *g_time_mid)) {
			throw "result becomes empty!";
		}
	} while (hausdorff(time_old, *g_time_mid) >= HSS_EPSILON);

	*X_mid = curve(*g_time_mid);
	*g_time_mid += time_procd;
#ifdef HSS_DEBUG
std::cout << std::endl << "mid:\t" << *X_mid << " at " << *g_time_mid << std::endl << std::endl;
#endif

	} catch(exception& e) {
		cout << "exception caught!\n" << e.what() << endl << endl;
		return false;
	}

	return true;
}
