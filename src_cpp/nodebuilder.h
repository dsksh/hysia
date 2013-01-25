
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

void putGrdTree(const int); 
void putGrdDTree(const int, const int);
void putJumpTree(const int); 
void putJumpDTree(const int, const int); 

void integrate(const float, const float, const float, const float);

#ifdef __cplusplus
}
#endif
