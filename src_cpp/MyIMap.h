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
#include "capd/map/Parser.h"

namespace capd{ 

class MyIMap : public capd::map::CnMap<capd::IMatrix,1>
{
public:
	MyIMap();
	MyIMap(const MyIMap&);
	~MyIMap();
};

} // the end of the namespace capd

#endif // _CAPD_MY_IMAP_H_ 
