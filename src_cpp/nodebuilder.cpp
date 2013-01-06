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
#include "util.h"
#include "nodebuilder.h"

using namespace std;
using namespace capd;
using namespace capd::map;

int g_dim;
DMapPtr g_der;
AuxMapPtr g_grd;
AuxMapPtr g_jump;
IVecPtr g_ivec;

typedef list<DerMap::NodeType *> NodeList;
NodeList g_stack;
int ivec_pos;

void init(const int dim)
{
	g_dim = dim;
	g_der = DMapPtr(new DerMap(dim, 1));
	g_grd = AuxMapPtr(new AuxMap(dim, 1));
	g_jump = AuxMapPtr(new AuxMap(dim, dim));
	g_stack.clear();
	g_ivec = IVecPtr(new IVector(dim));
	ivec_pos = 0;
}

/*DerMap *getIMap()
{
	return g_der.get();
}

const capd::IVector& getIVec()
{
	return *g_ivec;
}

const int getDim()
{
	return g_ivec->size();
}
*/

int putVariable(const char *name)
{
    return g_der->putVariable(name);
}

int setParam(const char *id, const double l, const double u)
{
	g_der->setParam(id, interval(l, u));
}

void putVarNode(const int index) 
{
	g_stack.push_front(g_der->createVarNode(index));
}

//void putScalarNode(const int val)
//{
//	g_stack.push_front(new ConsNodeEx<DerMap::ScalarType>(g_der->m_order, 
//														  DerMap::ScalarType(val)));
//}

void putScalarNode(const double l, const double u)
{
	g_stack.push_front(new ConsNodeEx<DerMap::ScalarType>(g_der->m_order, 
														  DerMap::ScalarType(l,u)));
}

void putSqrNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putSqrtNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrtNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putExpNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new ExpNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putLogNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new LogNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putSinNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SinNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putCosNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new CosNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putAtanNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AtanNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putAsinNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AsinNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putAcosNode()
{
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AcosNodeEx<DerMap::ScalarType>(g_der->m_order, l));
}

void putSumNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SumNodeEx<DerMap::ScalarType>(g_der->m_order, l, r));
}

void putDifNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DifNodeEx<DerMap::ScalarType>(g_der->m_order, l, r));
}

void putMulNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new MulNodeEx<DerMap::ScalarType>(g_der->m_order, l, r));
}

void putDivNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DivNodeEx<DerMap::ScalarType>(g_der->m_order, l, r));
}

void putPowNode()
{
	DerMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	DerMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new PowNodeEx<DerMap::ScalarType>(g_der->m_order, l, r));
}

void putDerTree(const int i)
{
	g_der->putTree(i, g_stack.front()); g_stack.pop_front();
}

void putDerDTree(const int i, const int j)
{
	g_der->putDTree(i, j, g_stack.front()); g_stack.pop_front();
}

void doneDerTree()
{
	g_der->doneTree();
}

void putGrdTree() 
{
	g_grd->putTree(0, g_stack.front()); g_stack.pop_front();
}

void putGrdDTree(const int j) 
{
	g_grd->putDTree(0, j, g_stack.front()); g_stack.pop_front();
}

void putJumpTree(const int i) 
{
	g_jump->putTree(i, g_stack.front()); g_stack.pop_front();
}

void putJumpDTree(const int i, const int j) 
{
	g_jump->putDTree(i, j, g_stack.front()); g_stack.pop_front();
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
	
	(*g_ivec)[ivec_pos] = (*t)(0);
	++ivec_pos;

	delete t;
}

void integrate(const float t_end, const float order, const float h_min, const float h_max)
{
	try{

	//getIMap()->compDiff();

	// The solver:
	ITaylor solver(*g_der, order, h_min);
	ITimeMap timeMap(solver);

	// The initial value:
	C0Rect2Set s(*g_ivec);

	cout << '{' << endl;
	dumpPipe1(cout, 0, s);

	// Here we start to integrate.
	// The time of integration:
	double T(t_end);
	timeMap.stopAfterStep(true);
	interval prevTime(0.);

	//try{
	do 
	{
		//IVector v = timeMap(T,s);
		timeMap.moveSet(T, s);

		interval stepMade(solver.getStep());
		const ITaylor::CurveType& curve = solver.getCurve();

		int grid(stepMade.rightBound()/h_max + 0.9999999999);
		if (grid==0) grid = 1;
//cout << stepMade.rightBound()/h_max << endl;
		for(int i=0;i<grid;++i)
		{
			interval subsetOfDomain = interval(i,i+1)*stepMade/grid;

			IVector v = curve(subsetOfDomain);
			dumpPipe1(cout, prevTime+subsetOfDomain, v);
		}
		prevTime = timeMap.getCurrentTime();

	} while(!timeMap.completed());

	dumpPipe1(cout, timeMap.getCurrentTime(), s, false);
	cout << "}" << endl;

	} catch(exception& e)
	{
		cout << "\n\nException caught!\n" << e.what() << endl << endl;
	}
}
