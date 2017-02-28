/*
 * $Source: /headas/headas/attitude/tasks/tristarid/MUtil.c,v $
 * $Revision: 1.7 $
 * $Date: 2005/08/29 12:35:32 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: MUtil.c,v $
 * Revision 1.7  2005/08/29 12:35:32  wiegand
 * Not necessary to determine second largest element of vector.
 *
 * Revision 1.6  2005/08/27 12:51:22  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.5  2005/08/09 19:52:37  drhunter
 * Considering final.
 *
 * Revision 1.4  2005/08/03 16:53:57  drhunter
 * Code changes to reflect new Java code. Still no many-to-many matching.
 * Significant differences in MAX_DELTA.
 *
 * Revision 1.3  2005/08/03 13:05:57  drhunter
 * Tristarid as of 9:00 AM, 8/3/05
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#include <math.h>
#include <stdio.h>

#include "MUtil.h"



double Math_max (const double a, const double b)
{
   if (a > b) return a;
   else       return b;
}


double Math_min (const double a, const double b)
{
   if (a < b) return a;
   else       return b;
}


int Math_signum (double x)
{
   if (x < 0)
      return -1;
   else if (x > 0)
      return 1;
   else
      return 0;
}


double Math_toDegrees (const double x)
{
   return x * (180 / Math_PI);
}


double Math_toRadians (const double x)
{
   return x * (Math_PI / 180);
}


double Math_v3dot (const double a[3], const double b[3])
{
   return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}


double Math_u3cosangle (const double a[3], const double b[3])
{
   return Math_v3dot(a, b);
}


double Math_u3angle (const double a[3], const double b[3])
{
   int x;
   double angle, d;
   double tmp = 0;
   for (x = 0; x < 3; x++) {
      double delta = a[x] - b[x];
      tmp += delta * delta;
   }
   d = sqrt(tmp);
   angle = 2 * asin(d / 2);
   return angle;
}


void Math_v3sub (double o[3], const double a[3], const double b[3])
{
   o[0] = a[0] - b[0];
   o[1] = a[1] - b[1];
   o[2] = a[2] - b[2];
}


void Math_v3normalize (double a[3])
{
   double norm2, invnorm;
   norm2 = a[0] * a[0] + a[1] * a[1] + a[2] * a[2];
   invnorm = 1 / sqrt(norm2);

   a[0] *= invnorm;
   a[1] *= invnorm;
   a[2] *= invnorm;
}


void Math_v3cross (double o[3], const double u[3], const double v[3])
{
   o[0] = u[1] * v[2] - u[2] * v[1];
   o[1] = -(u[0] * v[2] - u[2] * v[0]);
   o[2] = u[0] * v[1] - u[1] * v[0];
}


double Math_v3cosangle (double a[3], double b[3])
{
   Math_v3normalize(a);
   Math_v3normalize(b);
   return Math_u3cosangle(a, b);
}


void Math_rd2unit (const double ra, const double dec, double u[3])
{
   u[0] = cos(ra) * cos(dec);
   u[1] = sin(ra) * cos(dec);
   u[2] = sin(dec);
}


void Math_v3rdl (const double v[3], double rdl[3])
{
   const double EPSILON = 1e-9;

   double ra, dec, rho, rho2 = v[0] * v[0] + v[1] * v[1];
   double norm = sqrt(rho2 + v[2] * v[2]);
   if (norm == 0)
       return;

   rho = sqrt(rho2);
   ra = 0;
   dec = asin(v[2] / norm);

   if (rho > EPSILON) {
      double c = v[0] / rho;
      double s = v[1] / rho;

      if (fabs(s) < EPSILON)
         ra = (1 - c / fabs(c)) * Math_PI / 2;
      else
         ra = 2 * atan((1 - c) / s);

      if (ra > 2 * Math_PI)
         ra -= 2 * Math_PI;
      else if (ra < 0)
         ra += 2 * Math_PI;
   }

   rdl[0] = ra;
   rdl[1] = dec;
   rdl[2] = norm;
}


void Math_v3scale (const double i[3], const double scale, double o[3])
{
   o[0] = i[0] * scale;
   o[1] = i[1] * scale;
   o[2] = i[2] * scale;
}


void Math_v3outerx (const double u[3], const double v[3], double o[3][3])
{
   o[0][0] = u[0] * v[0];
   o[0][1] = u[0] * v[1];
   o[0][2] = u[0] * v[2];

   o[1][0] = u[1] * v[0];
   o[1][1] = u[1] * v[1];
   o[1][2] = u[1] * v[2];

   o[2][0] = u[2] * v[0];
   o[2][1] = u[2] * v[1];
   o[2][2] = u[2] * v[2];

}


void Math_minc (double m[3][3], double delta[3][3])
{
   int i, j;
   for (i = 0; i < 3; ++i)
      for (j = 0; j < 3; ++j)
         m[i][j] += delta[i][j];
}


/* sqrt(a^2 + b^2) without under/overflow. */
double Math_hypot (const double a, const double b)
{
   double r;

   if (fabs(a) > fabs(b)) {
      r = b/a;
      r = fabs(a) * sqrt(1 + r*r);
   }
   else if (b != 0) {
      r = a/b;
      r = fabs(b) * sqrt(1 + r*r);
   }
   else {
      r = 0.0;
   }
   return r;
}


void Math_quat2rm (const double q[4], double rm[3][3])
{
   /****************************
    * diagonal elements *
    ****************************/
   rm[0][0] = q[0] * q[0] - q[1] * q[1] - q[2] * q[2] + q[3] * q[3];
   rm[1][1] = -q[0] * q[0] + q[1] * q[1] - q[2] * q[2] + q[3] * q[3];
   rm[2][2] = -q[0] * q[0] - q[1] * q[1] + q[2] * q[2] + q[3] * q[3];

   /****************************
    * off diagonal *
    ****************************/
   rm[0][1] = 2.0 * (q[0] * q[1] + q[2] * q[3]);
   rm[1][0] = 2.0 * (q[0] * q[1] - q[2] * q[3]);

   rm[0][2] = 2.0 * (q[0] * q[2] - q[1] * q[3]);
   rm[2][0] = 2.0 * (q[0] * q[2] + q[1] * q[3]);

   rm[1][2] = 2.0 * (q[1] * q[2] + q[0] * q[3]);
   rm[2][1] = 2.0 * (q[1] * q[2] - q[0] * q[3]);
}


void Math_rotate (const double v[3], const double q[4], double p[3])
{
   double rm[3][3];

   Math_quat2rm(q, rm);

   Math_rotate2(v, rm, p);
}


void Math_rotate2 (const double v[3], double rm[3][3], double p[3])
{
   int i, j;
   for (i = 0; i < 3; ++i) {
      double y = 0;
      for (j = 0; j < 3; ++j)
         y += rm[i][j] * v[j];
      p[i] = y;
   }
}


void Math_qNormalize (double q[4])
{
   double norm2 = q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3];
   double invnorm = 1.0 / sqrt(norm2);

   q[0] *= invnorm;
   q[1] *= invnorm;
   q[2] *= invnorm;
   q[3] *= invnorm;
}


void Math_quatprod (const double q1[4], const double q2[4], double q[4])
{
   q[0] =  q2[3]*q1[0] + q2[2]*q1[1] - q2[1]*q1[2] + q2[0]*q1[3];
   q[1] = -q2[2]*q1[0] + q2[3]*q1[1] + q2[0]*q1[2] + q2[1]*q1[3];
   q[2] =  q2[1]*q1[0] - q2[0]*q1[1] + q2[3]*q1[2] + q2[2]*q1[3];
   q[3] = -q2[0]*q1[0] - q2[1]*q1[1] - q2[2]*q1[2] + q2[3]*q1[3];
}


void Math_createSystem (const double r[3], double u1[3], double u2[3])
{
   int i1, i2, i3;
   double s[3];

   /* biggest */
   i1 = fabs(r[1]) > fabs(r[0]) ? 1 : 0;
   i1 = fabs(r[2]) > fabs(r[i1]) ? 2 : i1;

   /* others */
   i2 = (i1 + 1) % 3;
   i3 = (i1 + 2) % 3;
#if 0
   if (fabs(r[i2]) < fabs(r[i3])) {
      int tmp = i2;
      i2 = i3;
      i3 = tmp;
   }
#endif

   s[i1] = 0;
   s[i2] = 0;
   s[i3] = 1;

   Math_v3cross(u1, r, s);
   Math_v3normalize(u1);

   Math_v3cross(u2, r, u1);
   Math_v3normalize(u1);
}

