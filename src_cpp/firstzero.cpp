#include <vector>

#include "Context.h"
#include "Parallelepiped.h"
#include "MapEx.h"
#include "util.h"

#include "simulatingHandler.h"

using namespace std;
using namespace capd;

inline bool reduceLower(DerMap& der, AuxMap& grd_h, AuxMapVec& grd_g,
						const ITaylor::CurveType& curve,
						const interval& time_init, const interval& time_procd,
						interval& time,
						//const int polar = 1)
						const int polar = 0)
{
	interval time_old;
	do {
		time_old = time;

		// differentiation of h and g wrt time
		// states over the time interval.
		const IVector dx( der(curve(time)) );
		const interval dh( grd_h.der()(1)*dx );
		vector<interval> dg(grd_g.size());
		for (int i(0); i < grd_g.size(); i++)
			dg[i] = grd_g[i]->der()(1)*dx;

g_context->cout << endl << "contracting lb:\t" << time+time_procd << endl;
if (time.left() < 0.) THROW("integration backward");

		//const interval offset(time.left());
		interval offset(time.left());
		time -= offset;

		// evaluate the guard h at the left bound.
		const interval h( grd_h(curve(offset))(1) );
g_context->cout << "offset:\t" << offset << endl;
g_context->cout << "h:\t" << h << endl;
g_context->cout << "dh:\t" << dh << endl;
//cout << "h':\t" << grd_h(curve(4.66414-time_procd.leftBound()))(1) << endl;

		// enforce the Box consistency
		interval *gamma_l(&time);
		interval *gamma_u(NULL);
		extDiv(-h, dh, gamma_l, gamma_u);

		if (gamma_l == NULL) {
			// gamma_u is also NULL
			return false;
		}
		else if (gamma_u != NULL) {
			// should not come here
			THROW("extdiv resulted in two intervals");
		}
g_context->cout << "contracted lb:\t" << time+offset+time_procd << endl;

		// evaluation of inequalities
		for (int i(0); i < grd_g.size(); i++) {
			// TODO
			offset += time.left();
			time -= time.left();

			// evaluate the guard g at the left bound.
			//const interval g( polar ?
			//		(*grd_g[i])(curve(offset))(1) - interval(0,INFINITY) :
			//		(*grd_g[i])(curve(offset))(1) + interval(0,INFINITY) );
			const interval g( polar ?
					(*grd_g[i])(curve(offset))(1) + interval(0,INFINITY) :
					(*grd_g[i])(curve(offset))(1) - interval(0,INFINITY) );
g_context->cout << polar << ", g[" << i << "]:\t" << g << endl;

			// enforce the Box consistency
			extDiv(-g, dg[i], gamma_l, gamma_u);
	
			if (gamma_l == NULL) {
				return false;
			}
			else if (gamma_u != NULL) {
				// should not come here
				THROW("extdiv resulted in two intervals");
			}
		}

		time += offset;

//g_context->cout << "hl: " << grd_h(curve(time.left())) << endl;
//g_context->cout << "hu: " << grd_h(curve(time.right())) << endl;

		// TODO: this is needed to fix a small perturbation caused by time - lb + lb
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
	//interval time_old(UNIVERSE);
	time = time.left();
	double d(INFINITY), d_old(INFINITY);

g_context->cout << endl << "time:\t" << time+time_procd << endl;
g_context->cout << endl << "time_init:\t" << time_init+time_procd << endl;

	do {

g_context->cout << endl << "verifying:\t" << time+time_procd << endl;
		
		// differentiate h
		const IVector  dx( der(curve(time)) );
g_context->cout << "dx: " << dx << endl;
g_context->cout << "x: " << curve(time) << endl;
		const interval dh( grd_h.der()(1)*dx );
g_context->cout << "dh: " << dh << " at " << time+time_procd << endl;
		if ( dh.contains((capd::TypeTraits<interval>::zero())) ) {
			THROW("zero in the derivative");
			//break;
		}

		// interval Newton
		interval contracted(time.left() - grd_h(curve(time.left()))(1) / dh);
		//intersection(time, contracted, contracted);
g_context->cout << "contracted:\t" << contracted+time_procd << endl;

		// inclusion test
		if (time.containsInInterior(contracted)) {
g_context->cout << "proved" << endl;
			time = contracted;
			return true;
		}

		// inflation
		d_old = d;
		//d = hausdorff(time_old, contracted);
		//time_old = contracted;
		d = hausdorff(time, contracted);

//g_context->cout << "mid:\t" << contracted.mid()+time_procd << endl;
		//time = time.mid() + g_params->tau*(contracted - time.mid())
		time = contracted.mid() + g_params->tau*(contracted - contracted.mid())
			 + interval(-g_params->abs_infl, g_params->abs_infl);
g_context->cout << "inflated:\t" << time+time_procd << endl;

		// TODO
		intersection(time, time_init, time);

	//} while (hausdorff(time_old, time) <= g_params->delta*d_old);
	//} while (hausdorff(time_old, time) <= g_params->delta*d);
	} while (d < g_params->delta*d_old);

cout << "failed" << endl;

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
			return false;
		}
		else if (gamma_u != NULL) {
			// should not come here
			THROW("extdiv resulted in two intervals");
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

	Parallelepiped pped = g_context->pped;

	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	const double time_l(time.rightBound());
	if (selected) g_context->time_l = time_l;

	interval reduced;

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
		reduced = time;
g_context->cout << endl << "step made (1): " << time+time_procd << endl;
		//const interval time_init(time);

		const ITaylor::CurveType& curve = solver.getCurve();

g_context->cout << "x:  " << curve(time) << endl;

//cout << "h':\t" << grd_h(curve(4.7-time_procd.leftBound()))(1) << endl;
//cout << "x:\t" << curve(2.5-time_procd.leftBound()) << endl;

		// reduce the lower bound
		//bool res( reduceLower(der, grd_h, grd_g, curve, time_init, time_procd, time) );
		bool res( reduceLower(der, grd_h, grd_g, curve, time, time_procd, reduced) );
//g_context->cout << "TIME: " << time+time_procd << endl;

		// dump the trajectory paving.
		if (selected && g_params->dump_interval > 0) {
		//if (!res) time = time_init;
		if (!res) reduced = time;
if (!res) {
		int grid(reduced.rightBound()/g_params->dump_interval + 0.9999999999);
 		if (grid==0) grid = 1;
		const double stepW(reduced.rightBound()/grid - 0.0000001);
 		for(int i(0); i<grid; ++i) {
 			const interval step( interval(i,i+1)*stepW );
 			IVector v = curve(step);

// TODO
//if (selected) {
 			printPipe(g_context->fout, step+time_procd, v);
			g_context->fout << ',' << endl;
//}
 		}
}
		}

		if (res)
			break;
		else if (timeMap.completed())
			return cEmpty;
		else {
			time_procd = time_l + timeMap.getCurrentTime();
			//dx_prev = IMatrix(capdPped);
			dx_prev = curve[solver.getStep()]*dx_prev;
			//dx_prev = midMatrix(curve[solver.getStep()])*dx_prev;
		}
	}

/*if (selected) {
Parallelepiped p(capdPped.get_B(), capdPped.get_r(), capdPped.get_x());
printPipe(g_context->fout, timeMap.getCurrentTime(), p);
g_context->fout << ',' << endl;
}
*/

	const ITaylor::CurveType& curve = solver.getCurve();

	// verification of the result
	if ( !verify(der, grd_h, curve, time, time_procd, reduced) ) {
		THROW("verification failed");
	}

	// reduce the upper bound
	if ( !reduceUpper(der, grd_h, curve, time, time_procd, reduced) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << reduced + time_procd << endl;

g_context->cout << "TIME: " << reduced << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

	// TODO
	if (selected && g_params->dump_interval > 0) {
		int grid(reduced.rightBound()/g_params->dump_interval + 0.9999999999);
		if (grid==0) grid = 1;
		const double stepW(reduced.rightBound()/grid - 0.0000001);
		for(int i(0); i<grid; ++i) {
			const interval step( interval(i,i+1)*stepW );
			IVector v = curve(step);
//if (selected) {
			printPipe(g_context->fout, step+time_procd, v);
			g_context->fout << ',' << endl;
//}
		}
	}

	for (int i(0); i < grd_g.size(); i++) {
		//if ( (*grd_g[i])(curve(reduced))(1).rightBound() >= 0 )
		if ( (*grd_g[i])(curve(reduced))(1).rightBound() <= 0 )
			THROW("inequality condition is not satisfied at last");
	}

//std::cout << curve[reduced.left()]*dx_prev << std::endl;

if (selected) {
	g_context->x = curve(reduced);
	g_context->x_left = curve(reduced.left());

	g_context->dx_phi = curve[reduced]*dx_prev;
	//g_context->dx_phi = curve[reduced];
//std::cout << "dx_phi" << g_context->dx_phi << std::endl;

	g_context->dt_phi = der(g_context->x);

	// TODO
	//grd_h(g_context->x);
	g_context->dh = grd_h.der()(1);
	//g_context->dh = grd_h[g_context->x](1);

	g_context->time = reduced + time_procd;
	//g_context->time += time_procd;

//g_context->pped = pped;

printPipe(g_context->fout, g_context->time, g_context->x);
g_context->fout << ',' << std::endl;
}

// TODO
reduced += time_procd;

	} 
	//catch(exception& e)
	CATCH
	{
cout << "caught" << endl;
		std::cerr << "exception caught! (1): " << eh_ex.what() << endl << endl;
		return cError;
	}

	cInterval res = {reduced.leftBound(), reduced.rightBound()};
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
g_context->cout << "time_init:\t" << time_init+time_procd << endl;
    interval time_old;
	do {
    	time_old = time_mid;

		const IVector  dx( der(curve(time_mid)) );
		const interval dh( grd_h.der()(1)*dx );
g_context->cout << "contracting:\t" << time_mid+time_procd << endl;
		//der(curve(time_mid.mid()));
		time_mid = time_mid.mid() - grd_h(curve(time_mid.mid()))(1) / dh;
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
	} while (hausdorff(time_old, time_mid) > g_params->epsilon &&
			 // TODO
			 time_mid.rightBound()-time_mid.leftBound() > g_params->epsilon );

	g_context->x_mid = curve(time_mid);
	time_mid += time_procd;
g_context->cout << endl << "mid:\t" << g_context->x_mid << " at " << time_mid << endl << endl;

	// TODO
	g_context->time_mid = time_mid;

	} 
	//catch (exception& e)
	CATCH 
	{
		std::cerr << "exception caught! (2): " << eh_ex.what() << endl << endl;
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

	interval reduced;

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
		//const interval time_init(time);
		reduced = time;
		const ITaylor::CurveType& curve = solver.getCurve();

		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, invariant, inv_norm, curve, time, time_procd, reduced) );
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
	//const interval time_init(time);

	// verification of the result
	if ( !verify(der, invariant, curve, time, time_procd, reduced) ) {
		THROW("verification failed");
	}

	// reduce the upper bound
	if ( !reduceUpper(der, invariant, curve, time, time_procd, reduced) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << reduced + time_procd << endl;

g_context->cout << "TIME: " << reduced << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
reduced += time_procd;
	} 
	CATCH {
		std::cerr << "exception caught! (3): " << eh_ex.what() << endl << endl;
		//cInterval err= {-1., -1.};
		//return err;
		return cError;
	}

	cInterval res = {reduced.leftBound(), reduced.rightBound()};
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
	AuxMap& ap = *loc->aps[apid];

	AuxMapVec ap_norm;
	ap_norm.push_back(loc->apNormals[apid]);

	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	double time_l(g_context->time.rightBound());
    interval time_procd(time_l);

	interval reduced;

	TRY {

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

	{
	// skip to the searched prefix time.

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	//timeMap.stopAfterStep(true);

	//IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
 		timeMap.moveSet(time_lower - time_l + 1e-8, capdPped); // TODO
		time_procd = time_l + timeMap.getCurrentTime();
		//dx_prev = IMatrix(capdPped);
		if (timeMap.completed()) break;
	}
g_context->cout << "moved to time_l: " << time_lower << " - " << time_l << " " << time_procd << endl;

	if (time_procd.leftBound() >= time_max)
		return cEmpty;

	time_l = time_procd.rightBound();
	}

	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	while (true) {

//cout << "integrate: " << time_max - time_l << endl;
		// integrate 1 step.
 		timeMap.moveSet(time_max - time_l, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (4): " << time/*+time_procd*/ << endl;
		//const interval time_init(time);
		reduced = time;
		const ITaylor::CurveType& curve = solver.getCurve();

		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, ap, ap_norm, curve, time, time_procd, reduced, polar) );
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
	//const interval time_init(time);

	// verification of the result
	if ( !verify(der, ap, curve, time, time_procd, reduced) ) {
		THROW("verification failed");
	}

/*	// reduce the upper bound
	if ( !reduceUpper(der, ap, curve, time_init, time_procd, time) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << time + time_procd << endl;
*/

g_context->cout << "TIME: " << reduced << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
reduced += time_procd;
	} 
	CATCH {
		std::cerr << "exception caught! (4): " << eh_ex.what() << endl << endl;
		return cError;
	}

	cInterval res = {reduced.leftBound(), reduced.rightBound()};
	return res;
}


/*int checkInvAtInitTime(const char *lid, const int iid)
{
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	AuxMap& invariant = *g_model->locs[lid]->invariant[iid];
	ITaylor solver(der, g_params->order, g_params->h_min);

	const IVector iv = g_context->pped.hull();
	interval lhs = invariant(iv)(1);
    // invariant: lhs > 0
    // negation holds strongly
	if (lhs.rightBound() < 0.)
		return 0;
	else
		return 1;
}*/

int checkPropAtInitTime(const char *lid, const int apid)
{
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	AuxMap& ap = *loc->aps[apid];
	ITaylor solver(der, g_params->order, g_params->h_min);

	const IVector iv = g_context->pped.hull();
	interval lhs = ap(iv)(1);
//std::cout << "lhs: " << lhs << std::endl;
	if (lhs.leftBound() >= 0.)
		return 1;
	else if (lhs.rightBound() <= 0.)
		return 0;
	else
		return -1;
}

int checkPropKind(const char *lid, const int apid, double *vl, double *vu)
{
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	AuxMap& ap_norm = *loc->apNormals[apid];
	ITaylor solver(der, g_params->order, g_params->h_min);

	IVector iv = g_context->pped.hull();
	interval lhs = ap_norm(iv)(1);
	for (int i=0; i < iv.dimension(); ++i)
		iv[i] = interval(-HUGE_VAL,HUGE_VAL);
	interval lhs1 = ap_norm(iv)(1);
//std::cout << "lhs: " << lhs << std::endl;
	if (0. < lhs.leftBound())
		return 1;
	else if (lhs.rightBound() < 0.)
		return 0;
	else if (lhs1.leftBound() == 0. && lhs1.rightBound() == 0.) { // TODO: constant function
		interval v = (*loc->aps[apid])(iv)(1);
		*vl = v.leftBound(); *vu = v.rightBound();
		return 2;
	}
	else
		return 4;
}

cInterval findPropExtremum(const char *lid, const int apid, 
						   const double time_lower, const double time_max)
{
g_context->cout << endl;
g_context->cout << "*** findPropExtremum: " << lid << "," << apid << endl;
g_context->cout << endl;

	int dim(g_model->dim);
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	AuxMap& ap_norm = *loc->apNormals[apid];
	AuxMapVec empty_vec;

	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	double time_l(g_context->time.rightBound());
    interval time_procd(time_l);

	interval reduced;

	TRY {

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

	{
	// skip to the searched prefix time.

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	//timeMap.stopAfterStep(true);

	//IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
 		timeMap.moveSet(time_lower - time_l + 1e-8, capdPped); // TODO
		time_procd = time_l + timeMap.getCurrentTime();
		//dx_prev = IMatrix(capdPped);
		if (timeMap.completed()) break;
	}
g_context->cout << "moved to time_l: " << time_lower << " - " << time_l << " " << time_procd << endl;

	if (time_procd.leftBound() >= time_max)
		return cEmpty;

	time_l = time_procd.rightBound();
	}

	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	while (true) {

//cout << "integrate: " << time_max - time_l << endl;
		// integrate 1 step.
 		timeMap.moveSet(time_max - time_l, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (4): " << time/*+time_procd*/ << endl;
		//const interval time_init(time);
		reduced = time;
		const ITaylor::CurveType& curve = solver.getCurve();

		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, ap_norm, empty_vec, curve, time, time_procd, reduced) );
		if (res)
			break;
		else if (timeMap.completed())
			return cEmpty;
		else {
			time_procd = time_l + timeMap.getCurrentTime();
			//dx_prev = IMatrix(capdPped);
		}
	}

/*	const ITaylor::CurveType& curve = solver.getCurve();
	//const interval time_init(time);

	// verification of the result
	if ( !verify(der, ap, curve, time, time_procd, reduced) ) {
		THROW("verification failed");
	}

	// reduce the upper bound
	if ( !reduceUpper(der, ap, curve, time_init, time_procd, time) )
		THROW("failed in reducing the upper bound");
g_context->cout << "contracted ub:\t" << time + time_procd << endl;
*/

g_context->cout << "TIME: " << reduced << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
reduced += time_procd;
	} 
	CATCH {
		std::cerr << "exception caught! (4): " << eh_ex.what() << endl << endl;
		return cError;
	}

	cInterval res = {reduced.leftBound(), reduced.rightBound()};
	return res;
}


// TODO: implementation of time shifting evaluation
cSigComp compareSignals(const char *lid, const int neg1, const int neg2, 
						const double st1, const double st2,
						const int apid1, const int apid2, 
						const double time_lower, const double time_max)
{
g_context->cout << endl;
g_context->cout << "*** compareSignals: " << lid << "," << apid1  << "," << apid2 << "; " << time_lower << ", " << time_max << endl;
g_context->cout << neg1 << ", " << neg2 << endl;
g_context->cout << endl;

	cSigComp result = {-1, cEmpty};

	int dim(g_model->dim);
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	DiffMap aps_diff(neg1, neg2, *loc->aps[apid1], *loc->aps[apid2]);
	AuxMapVec empty_vec;

	// initial value
	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	double time_l(g_context->time.rightBound());
    interval time_procd(time_l);

	interval reduced;

	TRY {

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

	{
	// skip to the searched prefix time.

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	//IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
		timeMap.moveSet(time_lower - time_l, capdPped); // TODO
		if (timeMap.completed()) break;
		//time_procd += interval(0.,1.) * solver.getStep();
		time_procd = timeMap.getCurrentTime();
	}
g_context->cout << "moved to time_l: " << time_lower << " - " << time_l << " " << time_procd << endl;

	time_l = solver.getStep().rightBound();

g_context->cout << "time_l: " << time_l + time_procd.rightBound() << endl;
	if (time_l + time_procd.rightBound() > time_max)
		return result;

	// check the polarity at the left bound.
	const ITaylor::CurveType& curve = solver.getCurve();

	//const interval lhs( aps_diff( curve(time_l) )(1) );
	const IVector x( curve(time_l) );
	const IVector mid( midVector(x) );
	//const interval lhs( aps_diff(mid) + aps_diff[x]*(x-mid) );
	const interval lhs( aps_diff(mid)(1) + (aps_diff[x]*(x-mid))(1) );

g_context->cout << "lhs: " << lhs << endl;
	if (lhs.leftBound() >= 0.) // TODO
		result.apid = apid2;
	else if (lhs.rightBound() <= 0.)
		result.apid = apid1;
	else
		result.apid = -1;
	}

	time_procd += time_l;
	time_l = time_procd.rightBound();

	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	while (true) {

//cout << "integrate: " << time_max - time_procd << endl;
		// integrate 1 step.
 		timeMap.moveSet(time_max - time_l, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (4): " << time+time_procd << endl;
		//const interval time_init(time);
		reduced = time;
		const ITaylor::CurveType& curve = solver.getCurve();

		// check the polarity
		if (result.apid < 0) {
			const interval lhs( aps_diff( curve(time.left()) )(1) );
g_context->cout << "lhs: " << lhs << endl;
			if (lhs.leftBound() >= 0.) // TODO
				result.apid = apid2;
			else if (lhs.rightBound() <= 0.)
				result.apid = apid1;
		}


		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, aps_diff, empty_vec, curve, time, time_procd, reduced) );
		if (res)
			break;
		else if (timeMap.completed()) {
g_context->cout << "completed" << endl;
			return result;
		} else {
			time_procd = time_l + timeMap.getCurrentTime();
			//dx_prev = IMatrix(capdPped);
		}
	}

g_context->cout << "TIME: " << reduced << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
reduced += time_procd;
	} 
	CATCH {
g_context->cout << "error" << endl;
		std::cerr << "exception caught! (4): " << eh_ex.what() << endl << endl;
		result.intv = cError;
		return result;
	}

	result.intv.l = reduced.leftBound();
	result.intv.u = reduced.rightBound();

	if (reduced.leftBound() - time_lower < g_params->epsilon) {
		// intersection segment
		result.apid = -1;
        // TODO
        result.intv.u += g_params->epsilon;
	}

g_context->cout << "result: " << result.apid << endl;

	return result;
}


cSigComp findIntersection(const char *lid, const int neg, const double st, const int apid, 
						  const double vl, const double vu,
						  const double time_lower, const double time_max)
{
g_context->cout << endl;
g_context->cout << "*** findIntersection: " << lid << "," << neg << ", " << apid << ", " << time_lower+st << ", " << time_max+st << endl;
g_context->cout << endl;

	cSigComp result = {-1, cEmpty};

	int dim(g_model->dim);
	Location *loc = g_model->locs[lid].get();
	DerMap& der = loc->der;
	TransMap aps_diff(neg, *loc->aps[apid], interval(vl,vu));
	AuxMapVec empty_vec;

	// initial value
	Parallelepiped pped = g_context->pped;
	interval time = g_context->time;
g_context->cout << "TIME0: " << time << endl;
	double time_l(g_context->time.rightBound());
    interval time_procd(time_l);

	interval reduced;

	TRY {

	// the initial value:
	CapdPped capdPped(pped.toCapdPped());

	{
	// skip to the searched prefix time.

	// the solver:
	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	//IMatrix dx_prev(IMatrix::Identity(dim));
	
	while (true) {
		timeMap.moveSet(time_lower+st - time_l, capdPped); // TODO
		if (timeMap.completed()) break;
		//time_procd += interval(0.,1.) * solver.getStep();
		time_procd = timeMap.getCurrentTime();
	}
g_context->cout << "moved to time_l: " << time_lower+st << " - " << time_l << " " << time_procd << endl;

	time_l = solver.getStep().rightBound();

g_context->cout << "time_l: " << time_l + time_procd.rightBound() << endl;
	if (time_l + time_procd.rightBound() > time_max+st)
		return result;

	// check the polarity at the left bound.
	const ITaylor::CurveType& curve = solver.getCurve();

	const IVector x( curve(time_l) );
g_context->cout << "x: " << x << endl;
	const IVector mid( midVector(x) );
g_context->cout << "mid: " << mid << endl;

g_context->cout << "lhs1: " << aps_diff(mid) << endl;
g_context->cout << "lhs2: " << aps_diff[x]*(x-mid) << endl;
	const interval lhs( aps_diff(mid)(1) + (aps_diff[x]*(x-mid))(1) );
g_context->cout << "lhs: " << lhs << endl;

	if (lhs.leftBound() >= 0.) // TODO
		result.apid = apid+1;
	else if (lhs.rightBound() <= 0.)
		result.apid = apid;
	else
		result.apid = -1;
	}

	time_procd += time_l;
	time_l = time_procd.rightBound();

	ITaylor solver(der, g_params->order, g_params->h_min);
	ITimeMap timeMap(solver);
	timeMap.stopAfterStep(true);

	while (true) {

		// integrate 1 step.
 		timeMap.moveSet(time_max+st - time_l, capdPped);

		time = interval(0,1)*solver.getStep();
g_context->cout << endl << "step made (4): " << time+time_procd << endl;
		reduced = time;
		const ITaylor::CurveType& curve = solver.getCurve();

		// check the polarity
		if (result.apid < 0) {
			const interval lhs( aps_diff( curve(time.left()) )(1) );
g_context->cout << "lhs: " << lhs << endl;
			if (lhs.leftBound() >= 0.) // TODO
				result.apid = apid+1;
			else if (lhs.rightBound() <= 0.)
				result.apid = apid;
		}


		IVector  dx( der(curve(time)) );
g_context->cout << "x:  " << curve(time) << endl;
g_context->cout << "dx: " << dx << endl; 

		// reduce the lower bound
		bool res( reduceLower(der, aps_diff, empty_vec, curve, time, time_procd, reduced) );
		if (res)
			break;
		else if (timeMap.completed()) {
g_context->cout << "completed" << endl;
			return result;
		} else {
			time_procd = time_l + timeMap.getCurrentTime();
		}
	}

g_context->cout << "TIME: " << reduced << endl;
g_context->cout << "GTIME: " << g_context->time << endl;

// TODO
reduced += time_procd;
	} 
	CATCH {
g_context->cout << "error" << endl;
		std::cerr << "exception caught! (4): " << eh_ex.what() << endl << endl;
		result.intv = cError;
		return result;
	}

	result.intv.l = reduced.leftBound();
	result.intv.u = reduced.rightBound();

	if (reduced.leftBound() - time_lower+st < g_params->epsilon) {
		// intersection segment
		result.apid = -1;
	}

	return result;
}
