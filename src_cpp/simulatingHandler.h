
#ifdef __cplusplus
extern "C" {
#endif

typedef struct cInterval
{
	double l;
	double u;
} cInterval; 

typedef struct cSigComp
{
	int apid;
	struct cInterval intv;
} cSigComp; 

/*const cInterval cEmpty = { 1., -1. };*/
extern const cInterval cEmpty;
extern const cInterval cError;

void simInitialize();
void simDispose();
void setParam(const char *, const char *, const double);
int checkInvAtInitTime(const char *, const int);
int checkPropAtInitTime(const char *, const int);
cInterval findPropFrontier(const char *, const int, const int, const double, const double);
cInterval findInvFrontier(const char *, const int);
cInterval findFirstZero(const int, const char *, const int);
int findFirstZeroMid(const char *, const int);
int checkPropKind(const char *, const int, double *, double *);
cInterval findPropExtremum(const char *, const int, const double, const double);
cSigComp compareSignals(const char *, const int, const int, const double, const double, const int, const int, const double, const double);
cSigComp findIntersection(const char *, const int, const double, const int, const double, const double, const double, const double);
void simulateJump(const char *, const int, const cInterval);
void simulateCont(const char *, const double);
cInterval valueAt(const double, const bool, const char *, const int);
void dumpConst(const bool, const double, const double, const double, const double);
void dumpAP(const char *, const int, const bool, const double, const double, const double);
void dumpBool(const bool, const double tl, const double tu);
void reportStep(int, const char *);
void printPped(int, int);
/*capd::interval getCurrentTime();*/
char *getDumpData();

#ifdef __cplusplus
}
#endif
