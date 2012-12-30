/*#include "capd/capdlib.h"*/

#ifdef __cplusplus
extern "C" {
#endif

void init(const int dim);
int putVariable(const char *name);
void putValue(const double l, const double u);
void putVarNode(const int index);
/*void putScalarNode(const int value);*/
void putScalarNode(const double value);

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

void putTree();
void putDTree();
void doneTree();

void integrate(const float, const float, const float, const float);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
capd::MyIMap *getIMap();
#endif
