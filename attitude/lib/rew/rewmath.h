/*
 * $Source: /headas/headas/attitude/lib/rew/rewmath.h,v $
 * $Revision: 1.2 $
 * $Date: 2012/11/26 18:23:35 $
 * 
 *   Math utility library.
 *
 * $Log: rewmath.h,v $
 * Revision 1.2  2012/11/26 18:23:35  rwiegand
 * Added functions to determine angle between 3d vectors and for quaternion
 * operations.
 *
 * Revision 1.1  2011/10/12 21:51:26  rwiegand
 * Moved math functions into library.
 *
 */

#ifndef LIB_REWMATH_H
#define LIB_REWMATH_H


double Math_PI();

double Math_toDegrees (const double x);

double Math_toRadians (const double x);

double Math_max (const double a, const double b);

double Math_min (const double a, const double b);

int Math_signum (double x);

double Math_acos (double radians);

double Math_v3dot (const double a[3], const double b[3]);

void Math_v3cross (double o[3], const double a[3], const double b[3]);

double Math_u3cosangle (const double a[3], const double b[3]);

double Math_u3angle (const double a[3], const double b[3]);

void Math_v3sub (double o[3], const double a[3], const double b[3]);

double Math_v3norm (const double a[3]);

void Math_v3normalize (double a[3]);

double Math_v3angle (const double a[3], const double b[3]);
double Math_v3cosangle (const double a[3], const double b[3]);

void Math_rd2unit (const double ra, const double dec, double u[3]);

void Math_v3rdl (const double v[3], double rdl[3]);

void Math_v3scale (const double i[3], const double scale, double o[3]);

void Math_v3outerx (const double u[3], const double v[3], double o[3][3]);

void Math_minc (double m[3][3], double delta[3][3]); 

double Math_hypot (const double a, const double b);
 
void Math_quat2rm (const double q[4], double rm[3][3]);

void Math_rotate (const double v[3], const double q[4], double p[3]);

void Math_rotate2 (const double v[3], double rm[3][3], double p[3]);

void Math_qNormalize (double q[4]); 

double Math_qangle(const double q[4]);

void Math_qxscalar(const double q0[4], double k, double q[4]);

void Math_qdelta(const double q0[4], const double q1[4], double q[4]);

void Math_quatprod(const double q1[4], const double q2[4], double q[4]);

void Math_createSystem(const double r[3], double u1[3], double u2[3]);


#endif

