#include "Context.h"
#include "Parallelepiped.h"
#include "MapEx.h"
#include "util.h"

#include <setjmp.h>

#include "simulatingHandler.h"

using namespace std;
using namespace capd;

//class Exception : public std::exception { };

#define EXCEPTION_HACK 1

#if EXCEPTION_HACK
jmp_buf eh_jb;
exception *eh_ex;
#endif

#if !EXCEPTION_HACK
#	define TRY try
#	define CATCH catch (const exception& eh_ex)
#	define THROW(msg) throw runtime_error(msg)
#else
	// emulation of exception handling to deal with a bug in Mac OS.
	// try ... catch() is also needed for the exceptions of CAPD etc.
#	define TRY if (setjmp(eh_jb) == 0) try
#	define CATCH catch (exception& e) { eh_ex = &e; goto EH_HANDLER; } else EH_HANDLER:
#	define THROW(msg) do { eh_ex = new runtime_error(msg); longjmp(eh_jb, 1); } while (0);
#endif

inline bool reduceLower(DerMap& der, AuxMap& grd_h, AuxMapVec& grd_g,
						const ITaylor::CurveType& curve,
						const interval& time_init, const interval& time_procd,
						interval& time,
						const int polar = 1)
{
	interval time_old;
	//int i(0);
	do {
		//if (i++ > 5) break;

		time_old = time;

		// states over the time interval.
		const IVector dx( der(curve(time)) );
		const interval dh( grd_h.der()(1)*dx );
		interval dg[grd_g.size()];
		for (int i(0); i < grd_g.size(); i++)
			dg[i] = grd_g[i]->der()(1)*dx;

g_context->cout << endl << "contracting lb:\t" << time+time_procd << endl;

		// evaluate the guard h at the left bound.
		const interval offset(time.left());
		//der(curve(offset));
		const interval h( grd_h(curve(offset))(1) );
		time -= offset;
g_context->cout << "offset:\t" << offset << endl;
g_context->cout << "h:\t" << h << endl;
//g_context->cout << "g:\t" << g << endl;

		interval *gamma_l(&time);
		interval *gamma_u(NULL);
		extDiv(-h, dh, gamma_l, gamma_u);
g_context->cout << "gl:\t" << *gamma_l << endl;

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

		for (int i(0); i < grd_g.size(); i++) {
			// evaluate the guard g at the left bound.
			const interval g( polar ?
					(*grd_g[i])(curve(offset))(1) + interval(0,INFINITY) :
					(*grd_g[i])(curve(offset))(1) - interval(0,INFINITY) );
g_context->cout << polar << ", g[" << i << "]:\t" << g << endl;

			extDiv(-g, dg[i], gamma_l, gamma_u);
	
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
		}

		time += offset;
		intersection(time_init, time, time);
g_context->cout << "contracted lb:\t" << time+time_procd << endl;

	} while (//time_old != time);
			 hausdorff(time_old, time) > g_params->epsilon);

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
		//interval time_tmp;
		//intersection(time_init, time, time_tmp);
		const IVector  dx( der(curve(time)) );
g_context->cout << "dx: " << dx << endl;
g_context->cout << "x: " << curve(time) << endl;
//g_context->cout << "time_tmp: " << time_tmp << endl;
		const interval dh( grd_h.der()(1)*dx );
g_context->cout << "dh: " << dh << " at " << time+time_procd << endl;
		if ( dh.contains((capd::TypeTraits<interval>::zero())) ) {
			// TODO
			THROW("zero in the derivative");
			break;
		}

		// interval Newton
		//der(curve(time.left()));
		// TODO
		interval contracted(time.left() - grd_h(curve(time.left()))(1) / dh);
		//interval contracted(time_tmp.left() - grd_h(curve(time_tmp.left()))(1) / dh);
		// TODO
		//intersection(time_init, contracted, contracted);
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

//g_context->cout << "mid:\t" << contracted.mid()+time_procd << endl;
		time = time.mid() + g_params->tau*(contracted - time.mid())
//		time = contracted.mid() + g_params->tau*(contracted - contracted.mid())
			 + interval(-g_params->abs_infl, g_params->abs_infl);
g_context->cout << "inflated:\t" << time+time_procd << endl;

		if ( time.contains((capd::TypeTraits<interval>::zero())) ) {
			// TODO
			THROW("zero in the time interval");
		}

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
g_context->cout << "time: " << time << endl;
g_context->cout << "dx: " << dx << endl;
		const interval dh = grd_h.der()(1)*dx;

		// state at the right bound.
		const interval offset(time.right());
		der( curve(offset) );
		const interval h( grd_h()(1) );
		time -= offset;

		interval *gamma_l(&time);
		interval *gamma_u(NULL);
		extDiv(-h, dh, gamma_l, gamma_u);

		if (gamma_l == NULL) {
			//THROW("gamma is empty");
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
			 hausdorff(time_old, time) > g_params->epsilon);

	return true;
}


cInterval findFirstZero(const int selected, const char *lid, const int eid)
{
g_context->cout << endl;
g_context->cout << "*** findFirstZero: " << lid << "," << eid << endl;
g_context->cout << endl;

	int dim(g_model->dim);
	LocPtr loc = g_model->locs[lid];
	DerMap& der = loc->der;
	EdgePtr edge = g_model->locs[lid]->edges[eid];
	AuxMap& grd_h = edge->grd_h;
	AuxMapVec& grd_g = edge->grd_g;
//der.setParameter("hoge", interval(0.));
//grd_h.setParameter("hoge", interval(0.));
//for (int i(0); i < grd_g.size(); i++)
//	grd_g[i]->setParameter("hoge", interval(0.));

	//Parallelepiped& pped = g_context->pped;
	//interval& time = g_context->time;
	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	const double time_l(time.rightBound());
	if (selected) g_context->time_l = time_l;

	TRY {

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

    interval time_procd(time_l);
	IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
		// integrate 1 step.
		//timeMap(g_params->t_max, p);
 		timeMap.moveSet(g_params->t_max, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (1): " << time+time_procd << endl;
		const interval time_init(time);
		const ITaylor::CurveType& curve = solver.getCurve();

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
		if (selected && g_params->dump_interval > 0) {
		if (!res) time = time_init;
if (!res) {
		int grid(time.rightBound()/g_params->dump_interval + 0.9999999999);
 		if (grid==0) grid = 1;
		const double stepW(time.rightBound()/grid - 0.0000001);
 		for(int i(0); i<grid; ++i) {
 			const interval step( interval(i,i+1)*stepW );
 			IVector v = curve(step);

// TODO
if (selected) {
 			printPipe(g_context->fout, step+time_procd, v);
			g_context->fout << ',' << endl;
}
 		}
}
		}

		if (res)
			break;
		else if (timeMap.completed())
			return cEmpty;
		else {
			time_procd = time_l + timeMap.getCurrentTime();
			dx_prev = IMatrix(capdPped);
		}
	}

/*if (selected) {
Parallelepiped p(capdPped.get_B(), capdPped.get_r(), capdPped.get_x());
printPipe(g_context->fout, timeMap.getCurrentTime(), p);
g_context->fout << ',' << endl;
}
*/

	const ITaylor::CurveType& curve = solver.getCurve();
	const interval time_init(time);

	// verification of the result
	if ( !verify(der, grd_h, curve, time_init, time_procd, time) ) {
		THROW("verification failed");

		// TODO
		//time_procd = time_l + timeMap.getCurrentTime();
		//dx_prev = IMatrix(P);
		//continue;
	}

	//if (time.right() == time_init.right()) {
	//	X  = curve(time);
	//	Dx_phi = curve[time]*dx_prev;
	//	Dt_phi = curve.derivative()(time);
	//	Dh = guard_h[X];
	//	time += time_procd;
	//	return true;
	//}

	// TODO
	//intersection(time_init, time, time);

	// reduce the upper bound
	if ( !reduceUpper(der, grd_h, curve, time_init, time_procd, time) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << time + time_procd << endl;

g_context->cout << "TIME: " << time << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

		// TODO
		if (selected && g_params->dump_interval > 0) {
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
		}

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
//g_context->pped = pped;

printPipe(g_context->fout, g_context->time, g_context->x);
g_context->fout << ',' << std::endl;
}

//	dumpPipe1(cout, timeMap.getCurrentTime(), s, false);
//	cout << "}" << endl;

// TODO
time += time_procd;

	} 
	//catch(exception& e)
	CATCH
	{
		std::cerr << "exception caught! (1)\n" << eh_ex->what() << endl << endl;
		return cEmpty;
	}

	cInterval res = {time.leftBound(), time.rightBound()};
	return res;
}


int findFirstZeroMid(const char *lid, const int eid)
{
g_context->cout << endl;
g_context->cout << "*** findFirstZeroMid: " << lid << "," << eid << endl;
g_context->cout << endl;

	LocPtr loc = g_model->locs[lid];
	DerMap& der = loc->der;
	AuxMap& grd_h = loc->edges[eid]->grd_h;

	const Parallelepiped& pped = g_context->pped;
	const interval& time = g_context->time;
	interval& time_mid = g_context->time_mid;
	double time_l = g_context->time_l;
g_context->cout << "TIME0 mid: " << time << endl;

	TRY {

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	// TODO
	//C0PpedSet capdPped(pped.x());
	C0Rect2Set capdPped(pped.x());

    interval time_procd(time_l);

	while (true) {
		// integrate 1 step.
		timeMap(g_params->t_max, capdPped);

		time_mid = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (2): " << time_procd + time_mid << endl;
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
	const interval time_init(time_mid);
    interval time_old;
	do {
    	time_old = time_mid;

		const IVector  dx( der(curve(time_mid)) );
		const interval dh( grd_h.der()(1)*dx );
g_context->cout << "contracting:\t" << time_mid+time_procd << endl;
		//der(curve(time_mid.mid()));
		time_mid = time_mid.mid() - 
				grd_h(curve(time_mid.mid()))(1) / dh;
		intersection(time_init, time_mid, time_mid);
g_context->cout << "contracted:\t" << time_mid+time_procd << endl;
g_context->cout << time << "-" << time_procd << " cap " << time_mid << endl;

g_context->cout << "time:\t" << time << endl;
g_context->cout << "time_procd:\t" << time_procd << endl;
g_context->cout << "time_mid:\t" << time_mid << endl;
		if (!intersection(time-time_procd, time_mid, time_mid)) {
			THROW("result becomes empty!");
			//continue;
			break;
		}
	} while (hausdorff(time_old, time_mid) > g_params->epsilon);

	g_context->x_mid = curve(time_mid);
	time_mid += time_procd;
g_context->cout << endl << "mid:\t" << g_context->x_mid << " at " << time_mid << endl << endl;

	} 
	//catch (exception& e)
	CATCH 
	{
		cerr << "exception caught! (2)\n" << eh_ex->what() << endl << endl;
		return false;
	}

	return true;
}


cInterval findInvFrontier(const char *lid, const int iid)
{
g_context->cout << endl;
g_context->cout << "*** findInvFrontier: " << lid << "," << iid << endl;
g_context->cout << endl;

	int dim(g_model->dim);
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	AuxMap& invariant = *g_model->locs[lid]->invariant[iid];

//der.setParameter("hoge", interval(0.));
//invariant.setParameter("hoge", interval(0.));

	//AuxMapVec inv_rest(g_model->locs[lid]->invariant);
	/*AuxMapVec::iterator it(inv_rest.begin());
	while (it != inv_rest.end()) {
		if (it->get() == &invariant)
			it = inv_rest.erase(it);
		else
			it++;
	}*/

	AuxMapVec inv_norm;
	inv_norm.push_back(g_model->locs[lid]->invNormal[iid]);
	//for (int i(0); i < inv_norm.size(); i++)
	//	inv_norm[i]->setParameter("hoge", interval(0.));

	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	const double time_l(time.rightBound());

	TRY {

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

    interval time_procd(time_l);
	IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {

		// integrate 1 step.
		//timeMap(g_params->t_max, p);
 		timeMap.moveSet(g_params->t_max, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (3): " << time+time_procd << endl;
		const interval time_init(time);
		const ITaylor::CurveType& curve = solver.getCurve();

		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, invariant, inv_norm, curve, time_init, time_procd, time) );
		if (res)
			break;
		else if (timeMap.completed())
			return cEmpty;
		else {
			time_procd = time_l + timeMap.getCurrentTime();
			dx_prev = IMatrix(capdPped);
		}
	}

	const ITaylor::CurveType& curve = solver.getCurve();
	const interval time_init(time);

	// verification of the result
	if ( !verify(der, invariant, curve, time_init, time_procd, time) ) {
		THROW("verification failed");
	}

	// reduce the upper bound
	if ( !reduceUpper(der, invariant, curve, time_init, time_procd, time) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << time + time_procd << endl;

g_context->cout << "TIME: " << time << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
time += time_procd;
	} 
	CATCH {
		std::cerr << "exception caught! (3)\n" << eh_ex->what() << endl << endl;
		//cInterval err= {-1., -1.};
		//return err;
		return cError;
	}

	cInterval res = {time.leftBound(), time.rightBound()};
	return res;
}


cInterval findPropFrontier(const char *lid, const int apid, const int polar,
						   const double time_lower, const double time_max)
{
g_context->cout << endl;
g_context->cout << "*** findPropFrontier: " << lid << "," << apid << endl;
g_context->cout << endl;

	int dim(g_model->dim);
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	//AuxMap& ap = *g_model->aps[apid];
	AuxMap& ap = *g_model->locs[lid]->aps[apid];

	AuxMapVec ap_norm;
	ap_norm.push_back(g_model->locs[lid]->apNormals[apid]);

	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	double time_l(g_context->time.rightBound());

	//interval time;

	TRY {

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

    interval time_procd(time_l);
	//IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
 		timeMap.moveSet(time_lower - time_l + 1e-8, capdPped); // TODO
		time_procd = time_l + timeMap.getCurrentTime();
		//dx_prev = IMatrix(capdPped);
		if (timeMap.completed()) break;
	}
g_context->cout << "moved to time_l: " << time_lower << " - " << time_l << " " << time_procd << endl;

	// TODO
	time_l = time_procd.rightBound();

	while (true) {

//g_context->cout << "integrate: " << time_max - time_l << endl;
		// integrate 1 step.
 		timeMap.moveSet(time_max - time_l, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (4): " << time+time_procd << endl;
		const interval time_init(time);
		const ITaylor::CurveType& curve = solver.getCurve();

		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, ap, ap_norm, curve, time_init, time_procd, time, polar) );
		if (res)
			break;
		else if (timeMap.completed())
			return cEmpty;
		else {
			time_procd = time_l + timeMap.getCurrentTime();
			//dx_prev = IMatrix(capdPped);
		}
	}

	const ITaylor::CurveType& curve = solver.getCurve();
	const interval time_init(time);

	// verification of the result
	if ( !verify(der, ap, curve, time_init, time_procd, time) ) {
		THROW("verification failed");
	}

	// reduce the upper bound
	if ( !reduceUpper(der, ap, curve, time_init, time_procd, time) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << time + time_procd << endl;

g_context->cout << "TIME: " << time << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
time += time_procd;
	} 
	CATCH {
		std::cerr << "exception caught! (4)\n" << eh_ex->what() << endl << endl;
		return cError;
	}

	cInterval res = {time.leftBound(), time.rightBound()};
	return res;
}
