#include <list>
#include "boost/shared_ptr.hpp"

#include "capd/map/Node.h"

#include "MyIMap.h"
#include "NodeEx.h"
#include "nodebuilder.h"

using namespace std;
using namespace capd;
using namespace capd::map;

//typedef boost::shared_ptr<MyIMap> MapPtr;
typedef auto_ptr<MyIMap> MapPtr;
typedef list<MyIMap::NodeType *> NodeList;

MapPtr g_map;
NodeList g_stack;

void init(const int dim) {
	g_map = MapPtr(new MyIMap(dim, 1));
	g_stack.clear();
}

MyIMap *getIMap() {
	return g_map.get();
}

void putVariable(const char *name) {
	g_map->putVariable(name);
}

void putVarNode(const int index) {
	g_stack.push_front(g_map->createVarNode(index));
}

void putScalarNode(const double val) {
	g_stack.push_front(new ConsNodeEx<MyIMap::ScalarType>(g_map->m_order, 
														  MyIMap::ScalarType(val)));
}

void putDifNode() {
	//MyIMap::NodeType *r(g_stack.pop_front()), *l(g_stack.pop_front());
	MyIMap::NodeType *r(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	g_stack.push_front(new DifNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putSinNode() {
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *r(new ConsNodeEx<MyIMap::ScalarType>(g_map->m_order, 
														   MyIMap::ScalarType(1.)));
	g_stack.push_front(new SinNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putCosNode() {
	MyIMap::NodeType *l(g_stack.front()); g_stack.pop_front();
	MyIMap::NodeType *r(new ConsNodeEx<MyIMap::ScalarType>(g_map->m_order, 
														   MyIMap::ScalarType(1.)));
	g_stack.push_front(new CosNodeEx<MyIMap::ScalarType>(g_map->m_order, l, r));
}

void putTree() {
	g_map->putTree(g_stack.front()); g_stack.pop_front();
}
