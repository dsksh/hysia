
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

void simInitialize();
void simDispose();
cInterval findInvFrontier(const char *, const int);
cInterval findFirstZero(const int, const char *, const int);
int findFirstZeroMid(const char *, const int);
void simulateJump(const char *, const int, const cInterval);
void simulateCont(const char *);
void reportStep(int, const char *);
void printPped(int, int);

#ifdef __cplusplus
}
#endif
