#ifndef _CAPD_MY_IMAP_H_ 
#define _CAPD_MY_IMAP_H_ 

#include <string>
#include <stdexcept>
#include <sstream>
#include <vector>

#include "capd/capdlib.h"

//#include "capd/map/BasicFunction.h"
//#include "capd/map/Function.h"
//#include "capd/map/C2Coeff.h"
//#include "capd/map/CnCoeff.h"
//#include "capd/map/SeriesContainer.h"

#include "capd/map/CnCoeff.hpp"
#include "capd/map/C2Coeff.hpp"
#include "capd/map/BasicFunction.hpp"
#include "capd/map/CnMap.h"
#include "capd/map/Node.h"

namespace capd{ 

class MyIMap : public capd::map::CnMap<capd::IMatrix,1>
{
public:
	MyIMap();
	MyIMap(int, int);
	MyIMap(const MyIMap&);
	~MyIMap();

	void setup();
	void setup1();

	int putVariable(const char *name);
	void putTree(capd::map::Node<ScalarType> *node);

	NodeType *createVarNode(int index);

	void compDiff();

	template<typename T>
	NodeType *compDiffNode(T& node, const int index);

private:
	int m_trees_idx;
};

} // the end of the namespace capd

#endif // _CAPD_MY_IMAP_H_ 
