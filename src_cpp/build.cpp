#include <list>

#include "capd/capdlib.h"
#include "capd/map/Node.h"
#include "capd/map/CnCoeff.hpp"
#include "capd/map/C2Coeff.hpp"
#include "capd/map/BasicFunction.hpp"
#include "capd/map/CnMap.h"
#include "capd/map/Parser.h"

#include "MapEx.h"
#include "NodeEx.h"
#include "Context.h"
#include "util.h"
#include "build.h"

using namespace std;
using namespace capd;
using namespace capd::map;

// the model instance
ModelPtr g_model;

typedef list<DerMap::NodeType *> NodeList;
NodeList g_stack;
int ivec_pos;

void init(const int dim)
{
	g_model = ModelPtr(new Model(dim));

	g_stack.clear();
	ivec_pos = 0;
}

int putVariable(const char *name)
{
    return g_model->der.putVariable(name);
}

void setParam(const char *id, const double l, const double u)
{
	g_model->der.setParam(id, interval(l, u));
}

void putVarNode(const int index) 
{
	g_stack.push_front(g_model->der.createVarNode(index));
}

//void putScalarNode(const int val)
//{
//	g_stack.push_front(new ConsNodeEx<DerMap::ScalarType>(g_model->der.m_order, 
//														  DerMap::ScalarType(val)));
//}

void putScalarNode(const double l, const double u)
{
	g_stack.push_front(new ConsNodeEx<DerMap::ScalarType>(g_model->der.m_order, 
														  DerMap::ScalarType(l,u)));
}

void putSqrNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putSqrtNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrtNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putExpNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new ExpNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putLogNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new LogNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putSinNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SinNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putCosNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new CosNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putAtanNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AtanNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putAsinNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AsinNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putAcosNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AcosNodeEx<DerMap::ScalarType>(g_model->der.m_order, l));
}

void putSumNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SumNodeEx<DerMap::ScalarType>(g_model->der.m_order, l, r));
}

void putDifNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DifNodeEx<DerMap::ScalarType>(g_model->der.m_order, l, r));
}

void putMulNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new MulNodeEx<DerMap::ScalarType>(g_model->der.m_order, l, r));
}

void putDivNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DivNodeEx<DerMap::ScalarType>(g_model->der.m_order, l, r));
}

void putPowNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new PowNodeEx<DerMap::ScalarType>(g_model->der.m_order, l, r));
}

void putDerTree(const int i)
{
	g_model->der.putTree(i, g_stack.front()); g_stack.pop_front();
}

void putDerDTree(const int i, const int j)
{
	g_model->der.putDTree(i, j, g_stack.front()); g_stack.pop_front();
}

void doneDerTree()
{
	g_model->der.doneTree();
}

void putGrdTree(const int s) 
{
	if (s == 0)
		//g_model->grd_h.putTree(0, g_stack.front());
		g_model->edges.back()->grd_h.putTree(0, g_stack.front());
	else
		g_model->edges.back()->grd_g.putTree(0, g_stack.front());
   	g_stack.pop_front();
}

void putGrdDTree(const int s, const int j) 
{
	if (s == 0)
		//g_model->grd_h.putDTree(0, j, g_stack.front());
		g_model->edges.back()->grd_h.putDTree(0, j, g_stack.front());
	else
		//g_model->grd_g.putDTree(0, j, g_stack.front());
		g_model->edges.back()->grd_g.putDTree(0, j, g_stack.front());
   	g_stack.pop_front();
}

void putJumpTree(const int i) 
{
	//g_model->jump.putTree(i, g_stack.front()); 
	g_model->edges.back()->jump.putTree(i, g_stack.front());
	g_stack.pop_front();
}

void putJumpDTree(const int i, const int j) 
{
	//g_model->jump.putDTree(i, j, g_stack.front()); 
	g_model->edges.back()->jump.putDTree(i, j, g_stack.front());
	g_stack.pop_front();
}

/*void putValue(const double l, const double u)
{
	(*g_ivec)[ivec_pos] = (l==u) ? l : interval(l, u);
	++ivec_pos;
}
*/

void putValue()
{
	DerMap::NodeType *t = g_stack.front();
	g_stack.pop_front();
	
	g_model->x_init[ivec_pos] = (*t)(0);
	++ivec_pos;

	delete t;
}
