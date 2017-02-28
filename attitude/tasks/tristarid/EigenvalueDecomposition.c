/*
 * $Source: /headas/headas/attitude/tasks/tristarid/EigenvalueDecomposition.c,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:50:53 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: EigenvalueDecomposition.c,v $
 * Revision 1.4  2005/08/27 12:50:53  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:05:57  drhunter
 * Tristarid as of 9:00 AM, 8/3/05
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#include <math.h>

#include "EigenvalueDecomposition.h"
#include "MUtil.h"


/* Symmetric Householder reduction to tridiagonal form */
void EigenvalueDecomposition_tred2(edPtr eVD) {

   /*  This is derived from the Algol procedures tred2 by
       Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
       Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
       Fortran subroutine in EISPACK. */

   int i, j;
   for (j = 0; j < eVD->n; j++) {
      eVD->d[j] = eVD->V[eVD->n-1][j];
   }

   /* Householder reduction to tridiagonal form. */
   for (i = eVD->n-1; i > 0; i--) {

      /* Scale to avoid under/overflow. */

      double scale = 0;
      double h = 0;
      int k;
      for (k = 0; k < i; k++) {
         scale = scale + fabs(eVD->d[k]);
      }
      if (scale == 0) {
         eVD->e[i] = eVD->d[i-1];
         for (j = 0; j < i; j++) {
            eVD->d[j] = eVD->V[i-1][j];
            eVD->V[i][j] = 0;
            eVD->V[j][i] = 0;
      }
      } else {
         double f, g, hh;

         /* Generate Householder vector. */
   
         for (k = 0; k < i; k++) {
            eVD->d[k] /= scale;
            h += eVD->d[k] * eVD->d[k];
         }
         f = eVD->d[i-1];
         g = sqrt(h);
         if (f > 0) {
            g = -g;
         }
         eVD->e[i] = scale * g;
         h = h - f * g;
         eVD->d[i-1] = f - g;
         for (j = 0; j < i; j++) {
            eVD->e[j] = 0.0;
         }

         /* Apply similarity transformation to remaining columns. */
   
         for (j = 0; j < i; j++) {
            f = eVD->d[j];
            eVD->V[j][i] = f;
            g = eVD->e[j] + eVD->V[j][j] * f;

            for (k = j+1; k <= i-1; k++) {
               g += eVD->V[k][j] * eVD->d[k];
               eVD->e[k] += eVD->V[k][j] * f;
            }
            eVD->e[j] = g;
         }
         f = 0;
         for (j = 0; j < i; j++) {
            eVD->e[j] /= h;
            f += eVD->e[j] * eVD->d[j];
         }
         hh = f / (h + h);
         for (j = 0; j < i; j++) {
            eVD->e[j] -= hh * eVD->d[j];
         }
         for (j = 0; j < i; j++) {
            f = eVD->d[j];
            g = eVD->e[j];
            for (k = j; k <= i-1; k++) {
               eVD->V[k][j] -= (f * eVD->e[k] + g * eVD->d[k]);
            }
            eVD->d[j] = eVD->V[i-1][j];
            eVD->V[i][j] = 0.0;
         }
      }
      eVD->d[i] = h;
   }

   /* Accumulate transformations. */

   for (i = 0; i < eVD->n-1; i++) {
      double h;
      int k;
      eVD->V[eVD->n-1][i] = eVD->V[i][i];
      eVD->V[i][i] = 1.0;
      h = eVD->d[i+1];
      if (h != 0.0) {
         for (k = 0; k <= i; k++) {
            eVD->d[k] = eVD->V[k][i+1] / h;
         }
         for (j = 0; j <= i; j++) {
            double g = 0.0;
            for (k = 0; k <= i; k++) {
               g += eVD->V[k][i+1] * eVD->V[k][j];
            }
            for (k = 0; k <= i; k++) {
               eVD->V[k][j] -= g * eVD->d[k];
            }
         }
      }
      for (k = 0; k <= i; k++) {
         eVD->V[k][i+1] = 0.0;
      }
   }
   for (j = 0; j < eVD->n; j++) {
      eVD->d[j] = eVD->V[eVD->n-1][j];
      eVD->V[eVD->n-1][j] = 0.0;
   }
   eVD->V[eVD->n-1][eVD->n-1] = 1.0;
   eVD->e[0] = 0.0;
}

/* Symmetric tridiagonal QL algorithm. */

void EigenvalueDecomposition_tql2 (edPtr eVD) {

/*  This is derived from the Algol procedures tql2, by
    Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    Fortran subroutine in EISPACK. */

   double f = 0.0;
   double tst1 = 0.0;
   double eps = pow(2.0,-52.0);
   int i, l;
   for (i = 1; i < eVD->n; i++) {
      eVD->e[i-1] = eVD->e[i];
   }
   eVD->e[eVD->n-1] = 0.0;

   for (l = 0; l < eVD->n; l++) {

      /* Find small subdiagonal element */

      int m = l;
      tst1 = Math_max(tst1, fabs(eVD->d[l]) + fabs(eVD->e[l]));
      while (m < eVD->n) {
         if (fabs(eVD->e[m]) <= eps*tst1) {
            break;
         }
         m++;
      }

      /* If m == l, d[l] is an eigenvalue,
         otherwise, iterate. */

      if (m > l) {
         int iter = 0;
         do {
            double c, c2, c3, dl1, el1, g, h, p, r, s, s2;
            iter = iter + 1;  /* Could check iteration count here. */

            /* Compute implicit shift */

            g = eVD->d[l];
            p = (eVD->d[l+1] - g) / (2.0 * eVD->e[l]);
            r = Math_hypot(p,1.0);
            if (p < 0) {
               r = -r;
            }
            eVD->d[l] = eVD->e[l] / (p + r);
            eVD->d[l+1] = eVD->e[l] * (p + r);
            dl1 = eVD->d[l+1];
            h = g - eVD->d[l];
            for (i = l+2; i < eVD->n; i++) {
               eVD->d[i] -= h;
            }
            f = f + h;

            /* Implicit QL transformation. */

            p = eVD->d[m];
            c = 1.0;
            c2 = c;
            c3 = c;
            el1 = eVD->e[l+1];
            s = 0.0;
            s2 = 0.0;
            for (i = m-1; i >= l; i--) {
               int k;
               c3 = c2;
               c2 = c;
               s2 = s;
               g = c * eVD->e[i];
               h = c * p;
               r = Math_hypot(p,eVD->e[i]);
               eVD->e[i+1] = s * r;
               s = eVD->e[i] / r;
               c = p / r;
               p = c * eVD->d[i] - s * g;
               eVD->d[i+1] = h + s * (c * g + s * eVD->d[i]);

               /* Accumulate transformation. */

               for (k = 0; k < eVD->n; k++) {
                  h = eVD->V[k][i+1];
                  eVD->V[k][i+1] = s * eVD->V[k][i] + c * h;
                  eVD->V[k][i] = c * eVD->V[k][i] - s * h;
               }
            }
            p = -s * s2 * c3 * el1 * eVD->e[l] / dl1;
            eVD->e[l] = s * p;
            eVD->d[l] = c * p;

            /* Check for convergence. */

         } while (fabs(eVD->e[l]) > eps*tst1);
      }
      eVD->d[l] = eVD->d[l] + f;
      eVD->e[l] = 0.0;
   }
  
   /* Sort eigenvalues and corresponding vectors. */

   for (i = 0; i < eVD->n-1; i++) {
      int k = i;
      double p = eVD->d[i];
      int j;
      for (j = i+1; j < eVD->n; j++) {
         if (eVD->d[j] < p) {
            k = j;
            p = eVD->d[j];
         }
      }
      if (k != i) {
         eVD->d[k] = eVD->d[i];
         eVD->d[i] = p;
         for (j = 0; j < eVD->n; j++) {
            p = eVD->V[j][i];
            eVD->V[j][i] = eVD->V[j][k];
            eVD->V[j][k] = p;
         }
      }
   }
}


/* Nonsymmetric reduction to Hessenberg form. */

void EigenvalueDecomposition_orthes (edPtr eVD) {
   
   /*  This is derived from the Algol procedures orthes and ortran,
       by Martin and Wilkinson, Handbook for Auto. Comp.,
       Vol.ii-Linear Algebra, and the corresponding
       Fortran subroutines in EISPACK. */

   int low = 0;
   int high = eVD->n-1;
   int i, j, m;
   for (m = low+1; m <= high-1; m++) {
   
      /* Scale column. */

      double scale = 0.0;

      for (i = m; i <= high; i++) {
         scale = scale + fabs(eVD->H[i][m-1]);
      }
      if (scale != 0.0) {
   
         /* Compute Householder transformation. */
   
         double g, h = 0.0;
         int i, j;
         for (i = high; i >= m; i--) {
            eVD->ort[i] = eVD->H[i][m-1]/scale;
            h += eVD->ort[i] * eVD->ort[i];
         }
         g = sqrt(h);
         if (eVD->ort[m] > 0) {
            g = -g;
         }
         h = h - eVD->ort[m] * g;
         eVD->ort[m] = eVD->ort[m] - g;

         /* Apply Householder similarity transformation
            H = (I-u*u'/h)*H*(I-u*u')/h) */

         for (j = m; j < eVD->n; j++) {
            double f = 0.0;
            for (i = high; i >= m; i--) {
               f += eVD->ort[i]*eVD->H[i][j];
            }
            f = f/h;
            for (i = m; i <= high; i++) {
               eVD->H[i][j] -= f*eVD->ort[i];
            }
        }
   
        for (i = 0; i <= high; i++) {
            double f = 0.0;
            for (j = high; j >= m; j--) {
               f += eVD->ort[j]*eVD->H[i][j];
            }
            f = f/h;
            for (j = m; j <= high; j++) {
               eVD->H[i][j] -= f*eVD->ort[j];
            }
         }
         eVD->ort[m] = scale*eVD->ort[m];
         eVD->H[m][m-1] = scale*g;
      }
   }
   
   /* Accumulate transformations (Algol's ortran). */

   for (i = 0; i < eVD->n; i++) {
      for (j = 0; j < eVD->n; j++) {
         eVD->V[i][j] = (i == j ? 1.0 : 0.0);
      }
   }

   for (m = high-1; m >= low+1; m--) {
      if (eVD->H[m][m-1] != 0.0) {
         for (i = m+1; i <= high; i++) {
            eVD->ort[i] = eVD->H[i][m-1];
         }
         for (j = m; j <= high; j++) {
            double g = 0.0;
            for (i = m; i <= high; i++) {
               g += eVD->ort[i] * eVD->V[i][j];
            }
            /* Double division avoids possible underflow */
            g = (g / eVD->ort[m]) / eVD->H[m][m-1];
            for (i = m; i <= high; i++) {
               eVD->V[i][j] += g * eVD->ort[i];
            }
         }
      }
   }
}


/* Complex scalar division. */
void cdiv(edPtr eVD, double xr, double xi, double yr, double yi) {
   double r,d;
   if (fabs(yr) > fabs(yi)) {
      r = yi/yr;
      d = yr + r*yi;
      eVD->cdivr = (xr + r*xi)/d;
      eVD->cdivi = (xi - r*xr)/d;
   } else {
      r = yr/yi;
      d = yi + r*yr;
      eVD->cdivr = (r*xr + xi)/d;
      eVD->cdivi = (r*xi - xr)/d;
   }
}


/* Nonsymmetric reduction from Hessenberg to real Schur form. */
void EigenvalueDecomposition_hqr2 (edPtr eVD) {
   
   /*  This is derived from the Algol procedure hqr2,
       by Martin and Wilkinson, Handbook for Auto. Comp.,
       Vol.ii-Linear Algebra, and the corresponding
       Fortran subroutine in EISPACK. */
   
   /* Initialize */
   
   int nn = eVD->n;
   int n = nn-1;
   int low = 0;
   int high = nn-1;
   double eps = pow(2.0,-52.0);
   double exshift = 0.0;
   double p=0,q=0,r=0,s=0,z=0,t,w,x,y;
   
   /* Store roots isolated by balanc and compute matrix norm */
   
   double norm = 0.0;
   int i, j;
   int iter = 0;
   for (i = 0; i < nn; i++) {
      int j;
      if ((i < low) || (i > high)) {
         eVD->d[i] = eVD->H[i][i];
         eVD->e[i] = 0.0;
      }
      for (j = Math_max(i-1,0); j < nn; j++) {
         norm = norm + fabs(eVD->H[i][j]);
      }
   }
   
   /* Outer loop over eigenvalue index */
   
   while (n >= low) {
   
      /* Look for single small sub-diagonal element */

      int l = n;
      while (l > low) {
         s = fabs(eVD->H[l-1][l-1]) + fabs(eVD->H[l][l]);
         if (s == 0.0) {
            s = norm;
         }
         if (fabs(eVD->H[l][l-1]) < eps * s) {
            break;
         }
         l--;
      }
       
      /* Check for convergence
         One root found */
   
      if (l == n) {
         eVD->H[n][n] = eVD->H[n][n] + exshift;
         eVD->d[n] = eVD->H[n][n];
         eVD->e[n] = 0.0;
         n--;
         iter = 0;

      /* Two roots found */

      } else if (l == n-1) {
         w = eVD->H[n][n-1] * eVD->H[n-1][n];
         p = (eVD->H[n-1][n-1] - eVD->H[n][n]) / 2.0;
         q = p * p + w;
         z = sqrt(fabs(q));
         eVD->H[n][n] = eVD->H[n][n] + exshift;
         eVD->H[n-1][n-1] = eVD->H[n-1][n-1] + exshift;
         x = eVD->H[n][n];
  
         /* Real pair */

         if (q >= 0) {
            int j;
            if (p >= 0) {
               z = p + z;
            } else {
               z = p - z;
            }
            eVD->d[n-1] = x + z;
            eVD->d[n] = eVD->d[n-1];
            if (z != 0.0) {
               eVD->d[n] = x - w / z;
            }
            eVD->e[n-1] = 0.0;
            eVD->e[n] = 0.0;
            x = eVD->H[n][n-1];
            s = fabs(x) + fabs(z);
            p = x / s;
            q = z / s;
            r = sqrt(p * p+q * q);
            p = p / r;
            q = q / r;
   
            /* Row modification */
   
            for (j = n-1; j < nn; j++) {
               z = eVD->H[n-1][j];
               eVD->H[n-1][j] = q * z + p * eVD->H[n][j];
               eVD->H[n][j] = q * eVD->H[n][j] - p * z;
            }
   
            /* Column modification */

            for (i = 0; i <= n; i++) {
               z = eVD->H[i][n-1];
               eVD->H[i][n-1] = q * z + p * eVD->H[i][n];
               eVD->H[i][n] = q * eVD->H[i][n] - p * z;
            }
   
            /* Accumulate transformations */
   
            for (i = low; i <= high; i++) {
               z = eVD->V[i][n-1];
               eVD->V[i][n-1] = q * z + p * eVD->V[i][n];
               eVD->V[i][n] = q * eVD->V[i][n] - p * z;
            }
   
         /* Complex pair */
   
         } else {
            eVD->d[n-1] = x + p;
            eVD->d[n] = x + p;
            eVD->e[n-1] = z;
            eVD->e[n] = -z;
         }
         n = n - 2;
         iter = 0;
  
         /* No convergence yet */
   
      } else {
         /* Form shift */
  
         int k, m;
         x = eVD->H[n][n];
         y = 0.0;
         w = 0.0;
         if (l < n) {
            y = eVD->H[n-1][n-1];
            w = eVD->H[n][n-1] * eVD->H[n-1][n];
         }

         /* Wilkinson's original ad hoc shift */

         if (iter == 10) {
            exshift += x;
            for (i = low; i <= n; i++) {
               eVD->H[i][i] -= x;
            }
            s = fabs(eVD->H[n][n-1]) + fabs(eVD->H[n-1][n-2]);
            x = y = 0.75 * s;
            w = -0.4375 * s * s;
         }

         /* MATLAB's new ad hoc shift */

         if (iter == 30) {
             s = (y - x) / 2.0;
             s = s * s + w;
             if (s > 0) {
                 s = sqrt(s);
                 if (y < x) {
                    s = -s;
                 }
                 s = x - w / ((y - x) / 2.0 + s);
                 for (i = low; i <= n; i++) {
                    eVD->H[i][i] -= s;
                 }
                 exshift += s;
                 x = y = w = 0.964;
             }
         }
   
         iter = iter + 1;   /* Could check iteration count here. */

         /* Look for two consecutive small sub-diagonal elements */

         m = n-2;
         while (m >= l) {
            z = eVD->H[m][m];
            r = x - z;
            s = y - z;
            p = (r * s - w) / eVD->H[m+1][m] + eVD->H[m][m+1];
            q = eVD->H[m+1][m+1] - z - r - s;
            r = eVD->H[m+2][m+1];
            s = fabs(p) + fabs(q) + fabs(r);
            p = p / s;
            q = q / s;
            r = r / s;
            if (m == l) {
               break;
            }
            if (fabs(eVD->H[m][m-1]) * (fabs(q) + fabs(r)) <
               eps * (fabs(p) * (fabs(eVD->H[m-1][m-1]) + fabs(z) +
               fabs(eVD->H[m+1][m+1])))) {
                  break;
            }
            m--;
         }

         for (i = m+2; i <= n; i++) {
            eVD->H[i][i-2] = 0.0;
            if (i > m+2) {
               eVD->H[i][i-3] = 0.0;
            }
         }
   
         /* Double QR step involving rows l:n and columns m:n */
   
         for (k = m; k <= n-1; k++) {
            int notlast = (k != n-1);
            if (k != m) {
               p = eVD->H[k][k-1];
               q = eVD->H[k+1][k-1];
               r = (notlast ? eVD->H[k+2][k-1] : 0.0);
               x = fabs(p) + fabs(q) + fabs(r);
               if (x != 0.0) {
                  p = p / x;
                  q = q / x;
                  r = r / x;
               }
            }
            if (x == 0.0) {
               break;
            }
            s = sqrt(p * p + q * q + r * r);
            if (p < 0) {
               s = -s;
            }
            if (s != 0) {
               int j;
               if (k != m) {
                  eVD->H[k][k-1] = -s * x;
               } else if (l != m) {
                  eVD->H[k][k-1] = -eVD->H[k][k-1];
               }
               p = p + s;
               x = p / s;
               y = q / s;
               z = r / s;
               q = q / p;
               r = r / p;
 
               /* Row modification */
  
               for (j = k; j < nn; j++) {
                  p = eVD->H[k][j] + q * eVD->H[k+1][j];
                  if (notlast) {
                     p = p + r * eVD->H[k+2][j];
                     eVD->H[k+2][j] = eVD->H[k+2][j] - p * z;
                  }
                  eVD->H[k][j] = eVD->H[k][j] - p * x;
                  eVD->H[k+1][j] = eVD->H[k+1][j] - p * y;
               }

               /* Column modification */

               for (i = 0; i <= Math_min(n,k+3); i++) {
                  p = x * eVD->H[i][k] + y * eVD->H[i][k+1];
                  if (notlast) {
                     p = p + z * eVD->H[i][k+2];
                     eVD->H[i][k+2] = eVD->H[i][k+2] - p * r;
                  }
                  eVD->H[i][k] = eVD->H[i][k] - p;
                  eVD->H[i][k+1] = eVD->H[i][k+1] - p * q;
               }

               /* Accumulate transformations */

               for (i = low; i <= high; i++) {
                  p = x * eVD->V[i][k] + y * eVD->V[i][k+1];
                  if (notlast) {
                     p = p + z * eVD->V[i][k+2];
                     eVD->V[i][k+2] = eVD->V[i][k+2] - p * r;
                  }
                  eVD->V[i][k] = eVD->V[i][k] - p;
                  eVD->V[i][k+1] = eVD->V[i][k+1] - p * q;
               }
            }  /* (s != 0) */
         }  /* k loop */
      }  /* check convergence */
   }  /* while (n >= low) */
   
   /* Backsubstitute to find vectors of upper triangular form */

   if (norm == 0.0) {
      return;
   }

   for (n = nn-1; n >= 0; n--) {
      p = eVD->d[n];
      q = eVD->e[n];

      /* Real vector */

      if (q == 0) {
         int l = n;
         eVD->H[n][n] = 1.0;
         for (i = n-1; i >= 0; i--) {
            int j;
            w = eVD->H[i][i] - p;
            r = 0.0;
            for (j = l; j <= n; j++) {
               r = r + eVD->H[i][j] * eVD->H[j][n];
            }
            if (eVD->e[i] < 0.0) {
               z = w;
               s = r;
            } else {
               l = i;
               if (eVD->e[i] == 0.0) {
                  if (w != 0.0) {
                     eVD->H[i][n] = -r / w;
                  } else {
                     eVD->H[i][n] = -r / (eps * norm);
                  }

               /* Solve real equations */

               } else {
                  x = eVD->H[i][i+1];
                  y = eVD->H[i+1][i];
                  q = (eVD->d[i] - p) * (eVD->d[i] - p) + eVD->e[i] * eVD->e[i];
                  t = (x * s - z * r) / q;
                  eVD->H[i][n] = t;
                  if (fabs(x) > fabs(z)) {
                     eVD->H[i+1][n] = (-r - w * t) / x;
                  } else {
                     eVD->H[i+1][n] = (-s - y * t) / z;
                  }
               }

               /* Overflow control */

               t = fabs(eVD->H[i][n]);
               if ((eps * t) * t > 1) {
                  for (j = i; j <= n; j++) {
                     eVD->H[j][n] = eVD->H[j][n] / t;
                  }
               }
            }
         }

      /* Complex vector */

      } else if (q < 0) {
         int l = n-1;

         /* Last vector component imaginary so matrix is triangular */

         if (fabs(eVD->H[n][n-1]) > fabs(eVD->H[n-1][n])) {
            eVD->H[n-1][n-1] = q / eVD->H[n][n-1];
            eVD->H[n-1][n] = -(eVD->H[n][n] - p) / eVD->H[n][n-1];
         } else {
            EigenvalueDecomposition_cdiv(eVD, 0.0,-eVD->H[n-1][n],eVD->H[n-1][n-1]-p,q);
            eVD->H[n-1][n-1] = eVD->cdivr;
            eVD->H[n-1][n] = eVD->cdivi;
         }
         eVD->H[n][n-1] = 0.0;
         eVD->H[n][n] = 1.0;
         for (i = n-2; i >= 0; i--) {
            double ra,sa,vr,vi;
            int j;
            ra = 0.0;
            sa = 0.0;
            for (j = l; j <= n; j++) {
               ra = ra + eVD->H[i][j] * eVD->H[j][n-1];
               sa = sa + eVD->H[i][j] * eVD->H[j][n];
            }
            w = eVD->H[i][i] - p;

            if (eVD->e[i] < 0.0) {
               z = w;
               r = ra;
               s = sa;
            } else {
               l = i;
               if (eVD->e[i] == 0) {
                  EigenvalueDecomposition_cdiv(eVD, -ra,-sa,w,q);
                  eVD->H[i][n-1] = eVD->cdivr;
                  eVD->H[i][n] = eVD->cdivi;
               } else {
   
                  /* Solve complex equations */

                  x = eVD->H[i][i+1];
                  y = eVD->H[i+1][i];
                  vr = (eVD->d[i] - p) * (eVD->d[i] - p) + eVD->e[i] * eVD->e[i] - q * q;
                  vi = (eVD->d[i] - p) * 2.0 * q;
                  if ((vr == 0.0) && (vi == 0.0)) {
                     vr = eps * norm * (fabs(w) + fabs(q) +
                     fabs(x) + fabs(y) + fabs(z));
                  }
                  EigenvalueDecomposition_cdiv(eVD, x*r-z*ra+q*sa, x*s-z*sa-q*ra, vr, vi);
                  eVD->H[i][n-1] = eVD->cdivr;
                  eVD->H[i][n] = eVD->cdivi;
                  if (fabs(x) > (fabs(z) + fabs(q))) {
                     eVD->H[i+1][n-1] = (-ra - w * eVD->H[i][n-1] + q * eVD->H[i][n]) / x;
                     eVD->H[i+1][n] = (-sa - w * eVD->H[i][n] - q * eVD->H[i][n-1]) / x;
                  } else {
                     EigenvalueDecomposition_cdiv(eVD, -r-y*eVD->H[i][n-1],-s-y*eVD->H[i][n],z,q);
                     eVD->H[i+1][n-1] = eVD->cdivr;
                     eVD->H[i+1][n] = eVD->cdivi;
                  }
               }

               /* Overflow control */

               t = Math_max(fabs(eVD->H[i][n-1]), fabs(eVD->H[i][n]));
               if ((eps * t) * t > 1) {
                  for (j = i; j <= n; j++) {
                     eVD->H[j][n-1] = eVD->H[j][n-1] / t;
                     eVD->H[j][n] = eVD->H[j][n] / t;
                  }
               }
            }
         }
      }
   }

   /* Vectors of isolated roots */

   for (i = 0; i < nn; i++) {
      if ((i < low) || (i > high)) {
         int j;
         for (j = i; j < nn; j++) {
            eVD->V[i][j] = eVD->H[i][j];
         }
      }
   }

   /* Back transformation to get eigenvectors of original matrix */

   for (j = nn-1; j >= low; j--) {
      for (i = low; i <= high; i++) {
         int k;
         z = 0.0;
         for (k = low; k <= Math_min(j,high); k++) {
            z = z + eVD->V[i][k] * eVD->H[k][j];
         }
         eVD->V[i][j] = z;
      }
   }
}


void EigenvalueDecomposition_cdiv(edPtr eVD, double xr, double xi, double yr, double yi) {
   double r, d;
   if (fabs(yr) > fabs(yi)) {
      r = yi/yr;
      d = yr + r*yi;
      eVD->cdivr = (xr + r*xi)/d;
      eVD->cdivi = (xi - r*xr)/d;
   }
   else {
      r = yr/yi;
      d = yi + r*yr;
      eVD->cdivr = (r*xr + xi)/d;
      eVD->cdivi = (r*xi - xr)/d;
   }
}

/* ------------------------
   Constructor
 * ------------------------ */

   /** Check for symmetry, then construct the eigenvalue decomposition
   @param A    Square matrix
   @return     Structure to access D and V.
   */

void EigenvalueDecomposition_construct (edPtr eVD, double A[4][4]) {

   int i, j, n = eVD->n = 4;

   eVD->issymmetric = 1;
   for (j = 0; (j < n) & eVD->issymmetric; j++) {
      for (i = 0; (i < n) & eVD->issymmetric; i++) {
         eVD->issymmetric = (A[i][j] == A[j][i]);
      }
   }

   if (eVD->issymmetric) {
      for (i = 0; i < n; i++) {
         for (j = 0; j < n; j++) {
            eVD->V[i][j] = A[i][j];
         }
      }

      /* Tridiagonalize */
      EigenvalueDecomposition_tred2(eVD);

      /* Diagonalize */
      EigenvalueDecomposition_tql2(eVD);

   } else {

      for (j = 0; j < n; j++) {
         for (i = 0; i < n; i++) {
            eVD->H[i][j] = A[i][j];
         }
      }

      /* Reduce to Hessenberg form */
      EigenvalueDecomposition_orthes(eVD);

      /* Reduce Hessenberg to real Schur form */
      EigenvalueDecomposition_hqr2(eVD);
   }
}

