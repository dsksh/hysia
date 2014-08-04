#ifndef _CAPD_MODEL_H_ 
#define _CAPD_MODEL_H_ 

#include <memory>
#include <string>
#include <utility>
#include <list>
#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>

#include "capd/capdlib.h"

namespace capd{ 

typedef boost::shared_ptr<AuxMap> AuxMapPtr;
typedef std::vector<AuxMapPtr> AuxMapVec;

struct Edge
{
public:
	std::string dest;
	AuxMap grd_h;
	AuxMapVec grd_g;
	AuxMap jump;

	/// constractor
	Edge(capd::DerMap& der, const std::string& destination)
	  : dest(destination),
	    //grd_h(der, der.getOrder()), 
	    grd_h(der, 1), 
	    //grd_g(der, der.getOrder()),
	    grd_g(),
		jump(der, der.dimension())
	{}
};

typedef boost::shared_ptr<Edge> EdgePtr;
//typedef std::map<std::string,EdgePtr> EdgeSet;
typedef std::vector<EdgePtr> EdgeSet;

struct Location
{
public:
	const std::string name;
	capd::DerMap der;
	AuxMapVec invariant;
	AuxMapVec invNormal;
	EdgeSet edges;

	AuxMapVec aps;
	AuxMapVec apNormals;

	/// constractor
	Location(const std::string n, const capd::DerMap& dm)
	  : name(),
	    //der(dm.dimension(), dm.getOrder()),
		der(dm),
	    //der(d, Order),
		invariant(),
		invNormal(),
		edges()
	{ }

};

typedef boost::shared_ptr<Location> LocPtr;
typedef std::map<std::string,LocPtr> LocSet;

struct Model
{
public:
	const int dim;

	capd::IVector x_init;
	capd::DerMap der_proto;

	LocSet locs;

	//AuxMapVec aps;

	/// constractor
	Model(const int d, const int np)
	  : dim(d),
		x_init(d),
		der_proto(d, 1, np)
	{ 
		// kludge: put dummy nodes to cheat the destractor.
		for (int i(0); i < dim; ++i) {
			der_proto.putTree(i, new capd::map::ConsNode<DerMap::ScalarType>(
						der_proto.getOrder(), 
						DerMap::ScalarType(0.,0.) ));
			for (int j(0); j < dim; ++j) 
				der_proto.putDTree(i, j, new capd::map::ConsNode<DerMap::ScalarType>( 
						der_proto.getOrder(), 
						DerMap::ScalarType(0.,0.) ));
		}               								  
	}
};

//typedef std::auto_ptr<Model> ModelPtr;
typedef boost::shared_ptr<Model> ModelPtr;
//typedef Model *ModelPtr;


struct SolvingParams
{
public:
	int debug;

	int order;
	double t_max;
	double h_min;
	//double h_max;
	double dump_interval;

	//std::string DumpFilename;

	double epsilon;
	double abs_infl;
	double delta;
	double tau;

	int char_mtx;
	int qr_thres;

	/// constractor
	SolvingParams()
	  : debug(0),
	  
	    order(20),
	    t_max(100),
	    h_min(0.01),
		//h_max(0.1)
		dump_interval(0.1),
		epsilon(1e-12),
		abs_infl(1e-12),
		delta(0.99),
		tau(1.1),
		char_mtx(-1),
	    qr_thres(100)
	{}
};

typedef boost::shared_ptr<SolvingParams> ParamsPtr;

} // the end of the namespace capd


extern capd::ModelPtr g_model;
extern capd::ParamsPtr g_params;

#endif // _CAPD_MODEL_H_ 
