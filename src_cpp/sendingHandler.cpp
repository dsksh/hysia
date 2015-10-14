#include <list>
#include <cstring>

#include "capd/capdlib.h"
#include "capd/map/Node.h"
#include "capd/map/CnCoeff.hpp"
#include "capd/map/C2Coeff.hpp"
#include "capd/map/BasicFunction.hpp"
#include "capd/map/CnMap.h"
#include "capd/map/Parser.h"

#include "MapEx.h"
#include "NodeEx.h"
#include "Model.h"
#include "util.h"
#include "sendingHandler.h"

using namespace std;
using namespace capd;
using namespace capd::map;

// the model instance
ModelPtr g_model;
ParamsPtr g_params;

typedef list<DerMap::NodeType *> NodeList;
NodeList g_stack;
int ivec_pos;

void init(const int dim, const int nparams)
{
	g_model = ModelPtr(new Model(dim, nparams));
	g_params = ParamsPtr(new SolvingParams());
	g_stack.clear();
	ivec_pos = 0;
}

int putVariable(const char *name)
{
    return g_model->der_proto.putVariable(name);
}

int putParam(const char *name)
{
    return g_model->der_proto.putParameter(name);
}

//int setParam(const char *id, const double l, const double u)
//{
//	return g_model->der_proto.putAndSetParameter(id, interval(l, u));
//}

void setDebug(const int debug)
{
	g_params->debug = debug;
}

void putVarNode(const int index) 
{
	g_stack.push_front(g_model->der_proto.createVarNode(index));
}

void putScalarNode(const double l, const double u)
{
	g_stack.push_front(new ConsNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), 
				DerMap::ScalarType(l,u)));
}

void putSqrNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putSqrtNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrtNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putExpNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new ExpNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putLogNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new LogNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putSinNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SinNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putCosNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new CosNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putAtanNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AtanNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putAsinNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AsinNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putAcosNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AcosNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l));
}

void putSumNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SumNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l, r));
}

void putDifNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DifNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l, r));
}

void putMulNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new MulNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l, r));
}

void putDivNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DivNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l, r));
}

void putPowNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new PowNodeEx<DerMap::ScalarType>(
				g_model->der_proto.getOrder(), l, r));
}

void putDerTree(const char *lid, const int i)
{
	g_model->locs[lid]->der.putTree(i, g_stack.front()); g_stack.pop_front();
}

void putDerDTree(const char *lid, const int i, const int j)
{
	g_model->locs[lid]->der.putDTree(i, j, g_stack.front()); g_stack.pop_front();
}

void doneDerTree(const char *lid)
{
	g_model->locs[lid]->der.doneTree();
}

void putInvTree(const char *lid, const int i)
{
	capd::DerMap& der(g_model->locs[lid]->der);
	AuxMapPtr itree(new AuxMap(der, 1));
	g_model->locs[lid]->invariant.push_back(itree);
	itree->putTree(0, g_stack.front());
   	g_stack.pop_front();
}

void putInvDTree(const char *lid, const int i, const int j)
{
	g_model->locs[lid]->invariant[i]->putDTree(0, j, g_stack.front()); 
	g_stack.pop_front();
}

void putInvNormTree(const char *lid, const int i)
{
	capd::DerMap& der(g_model->locs[lid]->der);
	AuxMapPtr itree(new AuxMap(der, 1));
	g_model->locs[lid]->invNormal.push_back(itree);
	itree->putTree(0, g_stack.front());
   	g_stack.pop_front();
}

void putInvNormDTree(const char *lid, const int i, const int j)
{
	g_model->locs[lid]->invNormal[i]->putDTree(0, j, g_stack.front()); g_stack.pop_front();
}

void putGrdTree(const char *lid, const int eid, const int s) 
{
	EdgePtr edge = g_model->locs[lid]->edges[eid];
	if (s == 0)
		edge->grd_h.putTree(0, g_stack.front());
	else {
		//g_model->locs[lid]->edges[eid]->grd_g.putTree(0, g_stack.front());
		capd::DerMap& der(g_model->locs[lid]->der);
		AuxMapPtr gg(new AuxMap(der, 1));
		edge->grd_g.push_back(gg);
		gg->putTree(0, g_stack.front());
	}
   	g_stack.pop_front();
}

void putGrdDTree(const char *lid, const int eid, const int s, const int j) 
{
	if (s == 0)
		g_model->locs[lid]->edges[eid]->grd_h.putDTree(0, j, g_stack.front());
	else
		g_model->locs[lid]->edges[eid]->grd_g[s-1]->putDTree(0, j, g_stack.front());
   	g_stack.pop_front();
}

//void putJumpTree(const char *lid, const char *dst, const int i) 
void putJumpTree(const char *lid, const int eid, const int i) 
{
	//AuxMap& jump = g_model->locs[lid]->edges[dst]->jump;
	AuxMap& jump = g_model->locs[lid]->edges[eid]->jump;
	DerMap::NodeType *n = g_stack.front();
	//g_model->locs[lid]->edges[dst]->jump.putTree(i, g_stack.front());
	jump.putTree(i, n);
	g_stack.pop_front();
}

//void putJumpDTree(const char *lid, const char *dst, const int i, const int j) 
void putJumpDTree(const char *lid, const int eid, const int i, const int j) 
{
	//g_model->locs[lid]->edges[dst]->jump.putDTree(i, j, g_stack.front());
	g_model->locs[lid]->edges[eid]->jump.putDTree(i, j, g_stack.front());
	g_stack.pop_front();
}

void setInitParam(const char *id, const double v)
{
	g_model->der_proto.setParameter(id, v);
}

void putInitValue()
{
	//DerMap::NodeType *t = g_stack.front();
	Model::NodePtr t(g_stack.front());
	g_stack.pop_front();
	
	//g_model->x_init[ivec_pos] = (*t)(0);
	g_model->x_init[ivec_pos] = t;
	++ivec_pos;

	//delete t;
}

void putEdge(const char *lid, const char *dst)
{
	//g_model->locs[lid]->edges.insert( pair<string,EdgePtr>(dst,
	//			EdgePtr(new Edge(g_model->locs[lid]->der, dst)) ));
	g_model->locs[lid]->edges.push_back( EdgePtr(new Edge(g_model->locs[lid]->der, dst)) );
}

void putLocation(const char *name)
{
	g_model->locs.insert( pair<string,LocPtr>(name, 
				LocPtr(new Location(name, g_model->der_proto)) ));
}

//void putAPTree(const int i)
void putAPTree(const char *lid, const int i)
{
	//capd::DerMap& der(g_model->der_proto);
	capd::DerMap& der(g_model->locs[lid]->der);
	AuxMapPtr ptree(new AuxMap(der, 1));
	g_model->locs[lid]->aps.push_back(ptree);
	ptree->putTree(0, g_stack.front());
   	g_stack.pop_front();
}

//void putAPDTree(const int i, const int j)
void putAPDTree(const char *lid, const int i, const int j)
{
	//g_model->aps[i]->putDTree(0, j, g_stack.front()); 
	g_model->locs[lid]->aps[i]->putDTree(0, j, g_stack.front()); 
	g_stack.pop_front();
}

void putAPNormTree(const char *lid, const int i)
{
	capd::DerMap& der(g_model->locs[lid]->der);
	AuxMapPtr ptree(new AuxMap(der, 1));
	g_model->locs[lid]->apNormals.push_back(ptree);
	ptree->putTree(0, g_stack.front());
   	g_stack.pop_front();
}

void putAPNormDTree(const char *lid, const int i, const int j)
{
	g_model->locs[lid]->apNormals[i]->putDTree(0, j, g_stack.front()); g_stack.pop_front();
}

void setSolvingParam(const char *id, const double value)
{
	if (strcmp(id, "order") == 0) {
		g_params->order = value;
	}
	else if (strcmp(id, "cm_thres") == 0) {
		g_params->cm_thres = value;
	}
	//else if (strcmp(id, "qr_thres") == 0) {
	//	g_params->qr_thres = value;
	//}
	//else if (strcmp(id, "char_mtx") == 0) {
	//	g_params->char_mtx = value;
	//}
	else if (strcmp(id, "t_max") == 0) {
		g_params->t_max = value;
	}
	else if (strcmp(id, "h_min") == 0) {
		g_params->h_min= value;
	}
	else if (strcmp(id, "epsilon") == 0) {
		g_params->epsilon = value;
	}
	else if (strcmp(id, "abs_infl") == 0) {
		g_params->abs_infl = value;
	}
	else if (strcmp(id, "delta") == 0) {
		g_params->delta = value;
	}
	else if (strcmp(id, "tau") == 0) {
		g_params->tau = value;
	}
	else if (strcmp(id, "dump_interval") == 0) {
		g_params->dump_interval = value;
	}
	else if (strcmp(id, "dump_to_file") == 0) {
		g_params->dump_to_file = value;
	}
	else if (strcmp(id, "dump_math") == 0) {
		g_params->dump_math = value;
	}
	else {
		std::cerr << "unknown param: " << id << std::endl;
		// TODO
	}
}
