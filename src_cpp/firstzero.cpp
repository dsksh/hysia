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

g_context->cout << endl << "contracting lb:\t" << time+time_procd << endl;

		// state at the left bound.
		const interval offset(time.left());
		//der(curve(offset));
		const interval h( grd_h(curve(offset))(1) );
		const interval g( grd_g(curve(offset))(1) + interval(0,INFINITY) );
		time -= offset;
g_context->cout << "offset:\t" << offset << endl;
g_context->cout << "h:\t" << h << endl;
g_context->cout << "g:\t" << g << endl;

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
g_context->cout << "contracted lb:\t" << time+time_procd << endl;

	} while (//time_old != time);
			 hausdorff(time_old, time) >= g_params->epsilon);

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

g_context->cout << endl << "time_init:\t" << time_init+time_procd << endl;

	do {

g_context->cout << endl << "verifying:\t" << time+time_procd << endl;
		// current state
		interval time_tmp;
		intersection(time_init, time, time_tmp);
		const IVector  dx( der(curve(time_tmp)) );
g_context->cout << "dx: " << dx << endl;
g_context->cout << "x: " << curve(time_tmp) << endl;
g_context->cout << "time_tmp: " << time_tmp << endl;
		const interval dh( grd_h.der()(1)*dx );
g_context->cout << "dh: " << dh << " at " << time_tmp+time_procd << endl;
		if ( dh.contains((capd::TypeTraits<interval>::zero())) ) {
			// TODO
			throw runtime_error("zero in the derivative");
			//std::cerr << "zero in the derivative" << endl;
			//break;
		}

		// interval Newton
		//der(curve(time.left()));
		interval contracted(time.left() - 
				grd_h(curve(time.left()))(1) / dh);
//				grd_h(curve(time_tmp.left()))(1) / dh);
g_context->cout << "contracted:\t" << contracted+time_procd << endl;
		if (time.containsInInterior(contracted)) {
g_context->cout << "proved" << endl;
			time = contracted;
			return true;
		}

		// inflation
		d_old = d;
		d = hausdorff(time_old, contracted);
		time_old = contracted;

		if (contracted.leftBound() == contracted.rightBound())
			contracted += interval(-g_params->h_min, g_params->h_min);

		//time = time.mid() + HSS_TAU*(contracted - time.mid());
g_context->cout << "lb:\t" << contracted.leftBound()+time_procd << endl;
g_context->cout << "rb:\t" << contracted.rightBound()+time_procd << endl;
g_context->cout << "mid:\t" << contracted.mid()+time_procd << endl;
g_context->cout << "mid':\t" << (contracted.leftBound()+contracted.rightBound())/2+time_procd << endl;
		time = contracted.mid() + g_params->tau*(contracted - contracted.mid());
g_context->cout << "inflated:\t" << time+time_procd << endl;

	} while (hausdorff(time_old, time) <= g_params->delta*d_old);

	return false;
}

inline bool reduceUpper(DerMap& der, AuxMap& grd_h, 
						const ITaylor::CurveType& curve,
					    const interval& time_init, const interval& time_procd,
					    interval& time)
{
	interval time_old;

	do {
g_context->cout << endl << "contracting rb: " << time+time_procd << endl;
//		time += time_procd;
//g_context->cout << endl << "contracting rb: " << time << endl;

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
			 hausdorff(time_old, time) >= g_params->epsilon);

	return true;
}


cInterval findFirstZero(const int selected, const char *lid, const char *dst)
{
	int dim(g_model->dim);
	DerMap& der = g_model->locs[lid]->der;
	AuxMap& grd_h = g_model->locs[lid]->edges[dst]->grd_h;
	AuxMap& grd_g = g_model->locs[lid]->edges[dst]->grd_g;

	//Parallelepiped& pped = g_context->pped;
	//interval& time = g_context->time;
Parallelepiped pped = g_context->pped;
interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	const double time_l(time.rightBound());
	if (selected) g_context->time_l = time_l;

	try{

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdPped p(pped.toCapdPped());
printPped(cout, pped);

    interval time_procd(time_l);
	IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
		// integrate 1 step.
		//timeMap(g_params->t_max, p);
 		timeMap.moveSet(g_params->t_max, p);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made: " << time+time_procd << endl;
		const ITaylor::CurveType& curve = solver.getCurve();
		const interval time_init(time);

		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 
//g_context->cout << "h:  " << grd_h()(1) << endl;
//g_context->cout << "dh: " << grd_h.der()(1)*dx << endl;
//g_context->cout << "g:  " << grd_g()(1) << endl;
//g_context->cout << "dg: " << grd_g.der()(1)*dx << endl;

		// reduce the lower bound
		bool res( reduceLower(der, grd_h, grd_g, curve, time_init, time_procd, time) );

		// dump the trajectory paving.
		if (!res) time = time_init;
		int grid(time.rightBound()/g_params->dump_interval + 0.9999999999);
 		if (grid==0) grid = 1;
		const double stepW(time.rightBound()/grid - 0.0000001);
 		for(int i(0); i<grid; ++i) {
 			const interval step( interval(i,i+1)*stepW );
 			IVector v = curve(step);
if (selected) {
 			printPipe(g_context->fout, step+time_procd, v);
			g_context->fout << ',' << endl;
}
 		}

		if (res)
			break;
		else if (timeMap.completed())
			return cEmpty;
		else {
			time_procd = time_l + timeMap.getCurrentTime();
			dx_prev = IMatrix(p);
		}
	}

	const ITaylor::CurveType& curve = solver.getCurve();
	const interval time_init(time);

	// verification of the result
	if ( !verify(der, grd_h, curve, time_init, time_procd, time) ) {
		throw runtime_error("verification failed");

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
		throw runtime_error("failed in reducing the upper bound");

g_context->cout << "contracted ub:\t" << time + time_procd << endl;

g_context->cout << "TIME: " << time << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

if (selected) {
	g_context->x = curve(time);
	g_context->x_left = curve(time.left());
//#ifndef HSS_SKIP_PPED_T_INF
	g_context->dx_phi = curve[time]*dx_prev;
//#else
//	g_context->dx_phi = curve[time.left()]*dx_prev;
//#endif
	//g_context->Dt_phi = curve.derivative()(time);
	g_context->dt_phi = der(g_context->x);
	g_context->dh = grd_h.der()(1);
g_context->time = time;
	g_context->time += time_procd;
g_context->pped = pped;
}

//	dumpPipe1(cout, timeMap.getCurrentTime(), s, false);
//	cout << "}" << endl;

	} catch(exception& e)
	{
		std::cerr << "exception caught!\n" << e.what() << endl << endl;
		return cEmpty;
	}

	cInterval res = {time.leftBound(), time.rightBound()};
	return res;
}


int findFirstZeroMid(const char *lid, const char *dst)
{
	DerMap& der = g_model->locs[lid]->der;
	AuxMap& grd_h = g_model->locs[lid]->edges[dst]->grd_h;

	const Parallelepiped& pped = g_context->pped;
	const interval& time = g_context->time;
	interval& time_mid = g_context->time_mid;
	double time_l = g_context->time_l;
g_context->cout << "TIME0 mid: " << time << endl;

	try{

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdBox p(pped.x());

    interval time_procd(time_l);

	while (true) {
		// integrate 1 step.
		timeMap(g_params->t_max, p);

		time_mid = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made: " << time_procd + time_mid << endl;
		if (intersection(time-time_procd, time_mid, time_mid))
			break;
		else if (timeMap.completed())
			return false;
		else
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
g_context->cout << "contracting:\t" << time_mid+time_procd << endl;
		//der(curve(time_mid.mid()));
		time_mid = time_mid.mid() - 
				grd_h(curve(time_mid.mid()))(1) / dh;
g_context->cout << "contracted:\t" << time_mid+time_procd << endl;
g_context->cout << time << "-" << time_procd << " cap " << time_mid << endl;

g_context->cout << "time:\t" << time << endl;
g_context->cout << "time_procd:\t" << time_procd << endl;
g_context->cout << "time_mid:\t" << time_mid << endl;
		if (!intersection(time-time_procd, time_mid, time_mid)) {
			//throw runtime_error("result becomes empty!");
			continue;
		}
	} while (hausdorff(time_old, time_mid) >= g_params->epsilon);

	g_context->x_mid = curve(time_mid);
	time_mid += time_procd;
g_context->cout << endl << "mid:\t" << g_context->x_mid << " at " << time_mid << endl << endl;

	} catch (exception& e) {
		cerr << "exception caught!\n" << e.what() << endl << endl;
		return false;
	}

	return true;
}
