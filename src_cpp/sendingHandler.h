
#ifdef __cplusplus
extern "C" {
#endif

void init(const int dim);
int putVariable(const char *);
int setParam(const char *, const double, const double);
void setDebug(const int);

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

void putDerTree(const char *, const int);
void putDerDTree(const char *, const int, const int);
void doneDerTree(const char *);

void putInvTree(const char *, const int);
void putInvDTree(const char *, const int, const int);

/*void putValue(const double l, const double u);*/
void putValue();

void putGrdTree(const char *, const int, const int); 
void putGrdDTree(const char *, const int, const int, const int);
void putJumpTree(const char *, const int, const int); 
void putJumpDTree(const char *, const int, const int, const int); 

void putEdge(const char *, const char *);
void putLocation(const char *);


void setSolvingParam(const char *, const double);

/*void integrate(const float, const float, const float, const float);*/

#ifdef __cplusplus
}
#endif
