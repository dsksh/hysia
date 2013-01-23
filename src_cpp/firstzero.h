
#ifdef __cplusplus
extern "C" {
#endif

void simInitialize();
int findFirstZero();
int findFirstZeroMid();
void simulateJump();

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include <memory>
#include "capd/capdlib.h"
#include "MapEx.h"

typedef std::auto_ptr<capd::Parallelepiped> PpedPtr;
typedef std::auto_ptr<double> DblPtr;
typedef std::auto_ptr<interval> IntPtr;
typedef std::auto_ptr<capd::IVector> IVecPtr;
typedef std::auto_ptr<capd::IMatrix> IMatPtr;

extern PpedPtr P;
extern IntPtr g_time;
extern IntPtr g_time_mid;
extern DblPtr g_time_l;
extern IVecPtr X;
extern IVecPtr X_mid;
extern IVecPtr X_left;
extern IMatPtr Dx;
extern IVecPtr Dt;
extern IVecPtr Dh;
#endif
