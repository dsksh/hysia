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
//typedef MyIMap *MapPtr;
typedef list<MyIMap::NodeType *> NodeList;
typedef auto_ptr<IVector> IVecPtr;
//typedef IVector *IVecPtr;

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

void putValue(const double val)
{
	(*g_ivec)[g_ivec_pos] = val;
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

void integrate()
{
	try{

	getIMap()->compDiff();

	ITaylor solver(*g_map, 20, 0.1);
	ITimeMap timeMap(solver);

	C0Rect2Set s(*g_ivec);

	// Here we start to integrate. The time of integration is set to T=10. 
	double T=1;
	//double T=0.1;
	timeMap.stopAfterStep(true);
	interval prevTime(0.);

	cout << '{' << endl;

	//try{
	do 
	{
		//IVector v = timeMap(T,s);
		timeMap.moveSet(T, s);
		//cout << s.show();
		//dumpPped(cout, s);
		//dumpPipe(cout, timeMap.getCurrentTime(), s);
		//cout << endl;

		interval stepMade = solver.getStep();
		//cout << endl << "step made: " << stepMade << endl;
		const ITaylor::CurveType& curve = solver.getCurve();

		//int grid=5;
		int grid=1;
		for(int i=0;i<grid;++i)
		{
			interval subsetOfDomain = interval(0,1)*stepMade;

			IVector v = curve(subsetOfDomain);
			dumpPipe1(cout, prevTime+subsetOfDomain, v);
		}
		prevTime = timeMap.getCurrentTime();
		//cout << endl << "current time: " << prevTime << endl << endl;

	} while(!timeMap.completed());

	} catch(exception& e)
	{
		cout << "\n\nException caught!\n" << e.what() << endl << endl;
	}

	cout << "{} }" << endl;
}
