#include "capd/capdlib.h"

#ifdef __cplusplus
extern "C" {
#endif

void init(const int dim);
void putVariable(const char *name);
void putVarNode(const int index);
void putScalarNode(const double value);
void putDifNode();
void putSinNode();
void putTree();

#ifdef __cplusplus
}
#endif

capd::MyIMap *getIMap();
