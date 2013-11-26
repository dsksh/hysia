
#ifdef __cplusplus
extern "C" {
#endif

typedef struct cInterval
{
	double l;
	double u;
} cInterval; 

const cInterval cEmpty = { 1., -1. };

void simInitialize();
void simDispose();
cInterval findFirstZero(const int, const char *, const char *);
int findFirstZeroMid(const char *, const char *);
void simulateJump(const char *, const char *, const cInterval);
void printPped(int, int);

#ifdef __cplusplus
}
#endif
