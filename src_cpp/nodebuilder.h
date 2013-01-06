
#ifdef __cplusplus
extern "C" {
#endif

void init(const int dim);
int putVariable(const char *);
int setParam(const char *, const double, const double);

void putVarNode(const int);
void putScalarNode(const double, const double);

void putSqrNode();
void putSqrtNode();
void putExpNode();
void putLogNode();
void putSinNode();
void putCosNode();
void putAtanNode();
void putAsinNode();
void putAcosNode();

void putSumNode();
void putDifNode();
void putMulNode();
void putDivNode();
void putPowNode();

void putDerTree(const int);
void putDerDTree(const int, const int);
void doneDerTree();

/*void putValue(const double l, const double u);*/
void putValue();

void putGrdTree(); 
void putGrdDTree(const int);
void putJumpTree(const int); 
void putJumpDTree(const int, const int); 

void integrate(const float, const float, const float, const float);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include <memory>
//#include "boost/shared_ptr.hpp"
#include "capd/capdlib.h"
#include "MapEx.h"

//typedef boost::shared_ptr<DerMap> DMapPtr;
typedef std::auto_ptr<capd::DerMap> DMapPtr;
//typedef capd::map::Function<capd::DerMap::VectorType> CapdFun;
//typedef capd::map::Map<capd::DerMap::MatrixType> CapdMap;
typedef std::auto_ptr<capd::AuxMap> AuxMapPtr;
typedef std::auto_ptr<capd::IVector> IVecPtr;

extern DMapPtr g_der;
extern AuxMapPtr g_grd;
extern AuxMapPtr g_jump;
extern IVecPtr g_ivec;
extern int g_dim;

//capd::DerMap *getIMap();
//const capd::IVector& getIVec();
//const int getDim();
#endif
