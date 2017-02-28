/*
 * $Source: /headas/headas/attitude/tasks/tristarid/MUtil.h,v $
 * $Revision: 1.5 $
 * $Date: 2005/08/27 12:51:25 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: MUtil.h,v $
 * Revision 1.5  2005/08/27 12:51:25  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.4  2005/08/09 19:53:32  drhunter
 * Considering final.
 *
 * Revision 1.3  2005/08/03 16:53:57  drhunter
 * Code changes to reflect new Java code. Still no many-to-many matching.
 * Significant differences in MAX_DELTA.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef TRISTARID_MUTIL_H
#define TRISTARID_MUTIL_H


static const double Math_PI = 3.1415926535897932384626433833;

double Math_toDegrees (const double x);

double Math_toRadians (const double x);

double Math_max (const double a, const double b);

double Math_min (const double a, const double b);

int Math_signum (double x);

double Math_v3dot (const double a[3], const double b[3]);

void Math_v3cross (double o[3], const double a[3], const double b[3]);

double Math_u3cosangle (const double a[3], const double b[3]);

double Math_u3angle (const double a[3], const double b[3]);

void Math_v3sub (double o[3], const double a[3], const double b[3]);

void Math_v3normalize (double a[3]);

double Math_v3cosangle (double a[3], double b[3]);

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

void Math_quatprod(const double q1[4], const double q2[4], double q[4]);

void Math_createSystem(const double r[3], double u1[3], double u2[3]);


#endif

