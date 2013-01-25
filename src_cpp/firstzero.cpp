#include "Context.h"
#include "Parallelepiped.h"
#include "MapEx.h"
#include "util.h"

#include "simulate.h"

using namespace std;
using namespace capd;

inline bool reduceLower(DerMap& der, AuxMap& grd_h, AuxMap& grd_g,
						const ITaylor::CurveType& curve,
						const interval& time_init, const interval& time_procd,
						interval& time)
{
	interval time_old;
	//int i(0);
	do {
		//if (i++ > 5) break;

		time_old = time;

		// current state
		const IVector dx( der(curve(time)) );
		const interval dh( grd_h.der()(1)*dx );
		const interval dg( grd_g.der()(1)*dx );

#ifdef HSS_DEBUG
std::cout << std::endl << "contracting lb:\t" << time << std::endl;
#endif

		// state at the left bound.
		const interval offset(time.left());
		//der(curve(offset));
		const interval h( grd_h(curve(offset))(1) );
		const interval g( grd_g(curve(offset))(1) + interval(0,INFINITY) );
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
			 hausdorff(time_old, time) >= g_context->Epsilon);

	return true;
}

inline bool verify(DerMap& der, AuxMap& grd_h,
				   const ITaylor::CurveType& curve,
				   const interval& time_init, const interval& time_procd,
				   interval& time)
{
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
		const IVector  dx( der(curve(time_tmp)) );
		const interval dh( grd_h.der()(1)*dx );
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
		//der(curve(time.left()));
		const interval contracted(time.left() - 
				grd_h(curve(time.left()))(1) / dh);
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
		time = contracted.mid() + g_context->Tau*(contracted - contracted.mid());
#ifdef HSS_DEBUG
std::cout << "inflated:\t" << time+time_procd << std::endl;
#endif

	} while (hausdorff(time_old, time) <= g_context->Tau*d_old);

	return false;
}

inline bool reduceUpper(DerMap& der, AuxMap& grd_h, 
						const ITaylor::CurveType& curve,
					    const interval& time_init, const interval& time_procd,
					    interval& time)
{
	interval time_old;

	do {
#ifdef HSS_DEBUG
//std::cout << std::endl << "contracting rb: " << time << std::endl;
#endif

		time_old = time;

		// current state.
		const IVector  dx = der(curve(time));
		const interval dh = grd_h.der()(1)*dx;

		// state at the right bound.
		const interval offset(time.right());
		der( curve(offset) );
		const interval h( grd_h()(1) );
		time -= offset;

		interval *gamma_l(&time);
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
			 hausdorff(time_old, time) >= g_context->Epsilon);

	return true;
}


int findFirstZero()
{
	int dim(g_model->dim);
	DerMap& der = g_model->der;
	AuxMap& grd_h = g_model->grd_h;
	AuxMap& grd_g = g_model->grd_g;

	Parallelepiped& pped = g_context->pped;
	interval& time = g_context->time;
	double time_l = g_context->time_l;

	try{

	// the solver:
	ITaylor solver(der, /**/20, /**/0.01);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	///C0Rect2Set s(ivec);
	CapdPped p(pped.toCapdPped());

	cout << '{' << endl;
	//dumpPipe1(cout, 0, s);

    interval time_procd(time_l);
	IMatrix dx_prev(dim,dim);

	while (true) {
		// integrate 1 step.
		timeMap(g_context->MaxTime, p);
		if (timeMap.completed())
			return false;

		time = interval(0,1)*solver.getStep();
#ifdef HSS_DEBUG
std::cout << std::endl << "step made: " << time+time_procd << std::endl;
#endif
		const ITaylor::CurveType& curve = solver.getCurve();
		const interval time_init(time);

		// TODO: inside/outside of the loop?
		IVector  dx( der(curve(time)) );
#ifdef HSS_DEBUG
std::cout << "x:  " << curve(time) << std::endl;
std::cout << "dx: " << dx << std::endl; 
//std::cout << "h:  " << grd_h()(1) << std::endl;
//std::cout << "dh: " << grd_h.der()(1)*dx << std::endl;
//std::cout << "g:  " << grd_g()(1) << std::endl;
//std::cout << "dg: " << grd_g.der()(1)*dx << std::endl;
#endif

		// reduce the lower bound
		if ( reduceLower(der, grd_h, grd_g,
					curve, time_init, time_procd, time) )
			break;

		time_procd = time_l + timeMap.getCurrentTime();
		dx_prev = IMatrix(p);
	}

	const ITaylor::CurveType& curve = solver.getCurve();
	const interval time_init(time);

//std::cout << "contracted:\t" << time+time_procd << std::endl;

	// verification of the result
	if ( !verify(der, grd_h, curve, time_init, time_procd, time) ) {
		throw "verification failed";

		// TODO
		//time_procd = time_l + timeMap.getCurrentTime();
		//dx_prev = IMatrix(P);
		//continue;
	}

	/*if (time.right() == time_init.right()) {
		X  = curve(time);
		Dx_phi = curve[time]*dx_prev;
		Dt_phi = curve.derivative()(time);
		Dh = guard_h[X];
		time += time_procd;
		return true;
	}*/

	// TODO
	//intersection(time_init, time, time);

	// reduce the upper bound
	if ( !reduceUpper(der, grd_h, curve, time_init, time_procd, time) )
		throw "failed in reducing the upper bound";

#ifdef HSS_DEBUG
std::cout << "contracted ub:\t" << time + time_procd << std::endl;
#endif

	g_context->x = curve(time);
	g_context->x_left = curve(time.left());
#ifndef HSS_SKIP_PPED_T_INF
	g_context->dx_phi = curve[time]*dx_prev;
#else
	g_context->dx_phi = curve[time.left()]*dx_prev;
#endif
	//g_context->Dt_phi = curve.derivative()(time);
	g_context->dt_phi = der(g_context->x);
	g_context->dh = grd_h.der()(1);
	g_context->time += time_procd;

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
	DerMap& der = g_model->der;
	AuxMap& grd_h = g_model->grd_h;

	const Parallelepiped& pped = g_context->pped;
	const interval& time = g_context->time;
	interval& time_mid = g_context->time_mid;
	double time_l = g_context->time_l;

	try{

	// the solver:
	ITaylor solver(der, /**/20, /**/0.01);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdBox p(pped.x());

    interval time_procd(time_l);

	while (true) {
		// integrate 1 step.
		timeMap(g_context->MaxTime, p);
		if (timeMap.completed())
			return false;

		time_mid = interval(0,1)*solver.getStep();
#ifdef HSS_DEBUG
std::cout << std::endl << "step made: " << time_procd + time_mid << std::endl;
#endif
		if (intersection(time-time_procd, time_mid, time_mid))
			break;

		time_procd = time_l+timeMap.getCurrentTime();
	}

	const ITaylor::CurveType& curve = solver.getCurve();

	// reduce with the interval Newton
    //interval contracted(time_mid);
    interval time_old;
	do {
    	time_old = time_mid;

		const IVector  dx( der(curve(time_mid)) );
		const interval dh( grd_h.der()(1)*dx );
#ifdef HSS_DEBUG
std::cout << "contracting:\t" << time_mid+time_procd << std::endl;
#endif
		//der(curve(time_mid.mid()));
		time_mid = time_mid.mid() - 
				grd_h(curve(time_mid.mid()))(1) / dh;
#ifdef HSS_DEBUG
std::cout << "contracted:\t" << time_mid+time_procd << std::endl;
#endif

		if (!intersection(time-time_procd, time_mid, time_mid)) {
			throw "result becomes empty!";
		}
	} while (hausdorff(time_old, time_mid) >= g_context->Epsilon);

	g_context->x_mid = curve(time_mid);
	time_mid += time_procd;
#ifdef HSS_DEBUG
std::cout << std::endl << "mid:\t" << g_context->x_mid << " at " << time_mid << std::endl << std::endl;
#endif

	} catch(exception& e) {
		cout << "exception caught!\n" << e.what() << endl << endl;
		return false;
	}

	return true;
}
