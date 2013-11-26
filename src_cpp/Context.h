#ifndef _CAPD_CONTEXT_H_ 
#define _CAPD_CONTEXT_H_ 

#include <memory>
#include <string>
#include <utility>
#include <list>
#include <map>

#include <boost/shared_ptr.hpp>

#include "capd/capdlib.h"

#include "MapEx.h"
#include "Parallelepiped.h"

#include "Model.h"

namespace capd{ 

struct Context
{
public:
	capd::Parallelepiped pped;
	interval time;
	interval time_mid;
	double time_l; // FIXME
	capd::IVector x;
	capd::IVector x_mid;
	capd::IVector x_left;
	capd::IMatrix dx_phi;
	capd::IVector dt_phi;
	capd::IVector dh;

	const double MaxTime;
	const int QrThres;

	const int PrintFreq;
	const std::string DumpFilename;

	const double Epsilon;
	const double Delta;
	const double Tau;

	std::ostream& cout;
	std::ostream& fout;

	/// constractor
	Context(const Model& m,
			std::ostream& co, std::ostream& fo,
			const double maxT = 100, const int qrThres = 100,
			const int freq = 100, const std::string dumpFile = "pped.dat",
			const double eps = 1e-8, const double delta = 0.99, const double tau = 1.1)
	  : pped( capd::IMatrix::Identity(m.dim), 
			  m.x_init - capd::vectalg::midVector(m.x_init), 
			  capd::vectalg::midVector(m.x_init) ),

		time(), time_mid(), time_l(0),
		x(m.dim), x_mid(m.dim), x_left(m.dim),
		dx_phi(m.dim,m.dim), dt_phi(m.dim), dh(m.dim),

	    MaxTime(maxT), QrThres(qrThres),
	    PrintFreq(freq), DumpFilename(dumpFile),
	    Epsilon(eps), Delta(delta), Tau(tau),

	    cout(co), fout(fo)
	{}
};

typedef std::auto_ptr<Context> CtxPtr;


} // the end of the namespace capd

extern capd::CtxPtr g_context;

#endif // _CAPD_CONTEXT_H_ 
