#ifndef _CAPD_CONTEXT_H_ 
#define _CAPD_CONTEXT_H_ 

#include <memory>

#include "capd/capdlib.h"

#include "MapEx.h"
#include "Parallelepiped.h"

namespace capd{ 

struct Context
{
public:
	capd::DerMap der;
	capd::AuxMap grd_h;
	capd::AuxMap grd_g;
	capd::AuxMap jump;
	capd::IVector x_init;
	int dim;

	capd::Parallelepiped pped;
	interval time;
	interval time_mid;
	double time_l;
	capd::IVector x;
	capd::IVector x_mid;
	capd::IVector x_left;
	capd::IMatrix dx_phi;
	capd::IVector dt_phi;
	capd::IVector dh;

	/// constractor
	Context(int d)
	  : dim(d),
		der(d, 1),
		grd_h(der, d, 1), grd_g(der, d, 1),
		jump(der, d, d),
		x_init(d), 

		pped(),
		time(), time_mid(), time_l(0),
		x(d), x_mid(d), x_left(d),
		dx_phi(d,d), dt_phi(d), dh(d)
	{
	}

	void initPped()
	{
		pped = Parallelepiped( capd::IMatrix::Identity(dim), 
					x_init - capd::vectalg::midVector(x_init), 
					capd::vectalg::midVector(x_init) );
	}
};

typedef std::auto_ptr<Context> CtxPtr;


} // the end of the namespace capd

extern capd::CtxPtr g_context;

#endif // _CAPD_CONTEXT_H_ 
