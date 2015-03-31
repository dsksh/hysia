
#ifdef __cplusplus
extern "C" {
#endif

typedef struct cInterval
{
	double l;
	double u;
} cInterval; 

/*const cInterval cEmpty = { 1., -1. };*/
extern const cInterval cEmpty;
extern const cInterval cError;

void simInitialize();
void simDispose();
void setParam(const char *, const char *, const double);
/*int checkProp(const char *, const int);*/
cInterval findInvFrontier(const char *, const int);
cInterval findPropFrontier(const char *, const int, const int, const double, const double);
cInterval findFirstZero(const int, const char *, const int);
int findFirstZeroMid(const char *, const int);
void simulateJump(const char *, const int, const cInterval);
void simulateCont(const char *);
void reportStep(int, const char *);
void printPped(int, int);
/*capd::interval getCurrentTime();*/
const char *getDumpData();

#ifdef __cplusplus
}
#endif
