/*
// xrrt_types.hh
//
// Global definitions needed by the XRRT library code
//
// Richard L Fink GSFC/631
// 1997/04/23
// 1997/09/16 Modified for XRRT 2.0 to move as much of the global definitions
//            back into their original classes as possible for information
//            hiding purposes. I also split up and changed the whole error
//            code/exception scheme. R. Fink
*/

#ifndef XRRT_TYPES_HH
#define XRRT_TYPES_HH

#include <sys/types.h>

#ifdef __cplusplus
#include <complex>
typedef std::complex<double> double_complex;
#endif

/*
// Global Random Number Generator
*/
#ifdef __cplusplus
extern "C" {
#endif
double xrrtrandom(void);
double xrrtgaussrandom(void);
void xrrtrandomseed(int seed);
#ifdef __cplusplus
}
#endif

/*
// GLOBAL DEFINES
*/
#define	NO_ERROR 0 

/*
// MIRROR CLASS TYPEDEFS
*/
typedef int          Count;
typedef int          SurfaceFunction;
typedef int          SurfaceScatterMode;
typedef double       AngleInRadians;
typedef double       RadiusInMM;
typedef double       FPDistanceInMM;
typedef double       DistanceInMM;
typedef int          ReflectTable;

/*
// COLLIMATOR CLASS TYPEDEFS
*/
typedef int          ColSurfaceFunction;
typedef int          ColSurfaceScatterMode;
typedef double       ColAngleInRadians;
typedef double       ColRadiusInMM;
typedef double       ColFPDistanceInMM;
typedef double       ColDistanceInMM;

/*
// OBSTRUCTION CLASS TYPEDEFS
*/
typedef double       VertexInMM;
#define MAX_VERTEX  9000.
#define MIN_VERTEX  -9000.

/*
// REFLECTION CLASS TYPEDEFS
*/
typedef double       TableEnergy;
typedef double       BinAngle;
typedef double       ReflectProb;
typedef float        ScatProb;

/*
// PHOTON CLASS TYPEDEFS
*/
typedef double       EnergyInKev;
typedef double       UnitVectorMag;

#endif	/* XRRT_TYPES_HH */
