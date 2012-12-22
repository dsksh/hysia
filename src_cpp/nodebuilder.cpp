#include <list>
#include <memory>
//#include "boost/shared_ptr.hpp"

#include "capd/map/Node.h"

#include "MyIMap.h"
#include "NodeEx.h"
#include "util.h"
#include "nodebuilder.h"

using namespace std;
using namespace capd;
using namespace capd::map;

//typedef boost::shared_ptr<MyIMap> MapPtr;
typedef auto_ptr<MyIMap> MapPtr;
typedef list<MyIMap::NodeType *> NodeList;
typedef auto_ptr<IVector> IVecPtr;

MapPtr g_map;
NodeList g_stack;
IVecPtr g_ivec;
int g_ivec_pos;

void init(const int dim)
{
	g_map = MapPtr(new MyIMap(dim, 1));
	g_stack.clear();
	g_ivec = IVecPtr(new IVector(dim));
	g_ivec_pos = 0;
}

MyIMap *getIMap()
{
	return g_map.get();
}

int putVariable(const char *name)
{
	return g_map->putVariable(name);
}

void putValue(const double l, const double u)
{
	(*g_ivec)[g_ivec_pos] = (l==u) ? l : interval(l, u);
	++g_ivec_pos;
}

void putVarNode(const int index) 
{
	g_stack.push_front(g_map->createVarNode(index));
}

//void putScalarNode(const int val)
//{
//	g_stack.push_front(new ConsNodeEx<MyIMap::ScalarType>(g_map->m_order, 
//														  MyIMap::ScalarType(val)));
//}

void putScalarNode(const double val)
{
	g_stack.push_front(new ConsNodeEx<MyIMap::ScalarType>(g_map->m_order, 
														  MyIMap::ScalarType(val)));
}

void putSqrNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putSqrtNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SqrtNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putExpNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new ExpNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putLogNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new LogNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putSinNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SinNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putCosNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new CosNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putAtanNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AtanNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putAsinNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AsinNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putAcosNode()
{
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new AcosNodeEx<MyIMap::ScalarType>(g_map->m_order, l));
}

void putSumNode()
{
	MyIMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new SumNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putDifNode()
{
	MyIMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DifNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putMulNode()
{
	MyIMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new MulNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putDivNode()
{
	MyIMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DivNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putPowNode()
{
	MyIMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new PowNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putTree()
{
	g_map->putTree(g_stack.front()); g_stack.pop_front();
}


void integrate(const float t_end, const float order, const float h_min, const float h_max)
{
	try{

	getIMap()->compDiff();

	// The solver:
	ITaylor solver(*g_map, order, h_min);
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
