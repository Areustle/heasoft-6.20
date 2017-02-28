/* Code found through Netlib.
http://www.netlib.org/c/numcomp-free-c

Name        : svd.c.Z (8704 bytes)
Where       : in pub/C-numanal on ftp.usc.edu
Description : SVD based on pascal from J. C. Nash book
Author      : Bryant Marks (bryant@sioux.stanford.edu)
              Brian Collett (bcollett@hamilton.edu)
Version     : 14 April 1993 
*/

/*
*       This is a translation of the Singular Value Decomposition algorithm
of
*       Golub and Reinsch (Numerische Mathematik 14(1970) pp403-470) from  
      
*       Algol 60 to C. The routine takes a single matrix (A) and computes
two
*       additional matrices (U and V) and a vector W such that
*               A = UWV'
*       The calling sequence is
*       svd(a,m,n,u,w,v,eps,flags)
*       where
*       a is the original, m by n matrix
*       u is the upper resultant
*       w is the vector of singular values
*       v is the lower resultant
*       eps is a convergence test constant
*       flags tells what to do, values:-
*                                       0 produce only w
*                                       1 produce w and u
*                                       2 produce w and v
*                                       3 produce u, w, and v.
*       NOTE m must be greater than n or an error is signaled
*
*       BC 5/11/87
*       Moved to 0->n-1 indices for vectors BC 5/12/87
*/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"

#define WITHV   1
#define WITHU   2
#define TINY    1.0E-38
void parray (char *, double *, int, int);

int svd(a,m,n,u,w,v,eps,flags,temp)
double *a,                      /* The original matrix [m,n] */
       *u,                      /* The new upper matrix [m,n] */
       *v,                      /* The new lower matrix [m,n] */
       *w,                      /* The vector of singular values [n] */
       *temp,                   /* A temporary vector */
        eps;                    /* Convergence factor */
int m,                          /* Number of rows */
    n,                          /* Number of columns (n <= m) */
    flags;                      /* Flags controlling what gets computed. */
{
  /* extern double TINY;  */   /* The smallest representable value */
        int i,j,k,l,l1;         /* Mostly loop variables */
        double tol = TINY / eps,/* tells about machine tolerance */
               c,f,g,h,s,x,y,z; /* Temporaries */
	l = 0;
        if (m < n) 
                return(-1);
/*
*       First copy a into u.
*/
        for (i = 0; i < m * n; ++i) {
             u[i] = a[i];
             }
parray("initial array",u,n,m);
/*
*       Reduce the u matrix to bidiagonal form with Householder transforms.
*/
        g = (x = 0.0);
        for(i = 0; i < n; ++i) {
                temp[i] = g;
                s = 0.0;
                l = i + 1;
                for (j = i; j < m; j++) {
                        s += u[j * n + i] * u[j * n + i];
                        }
                if (s < tol) {
                        g = 0.0;
                        }
                else {
                        f = u[i * n + i];
                        g = (f < 0.0) ? sqrt(s) : -sqrt(s);
                        h = f * g - s;
                        u[i * n + i] = f - g;
                        for(j = l; j < n; ++j) {
                                s = 0.0;
                                for(k = i; k < m; ++k) {
                                        s += u[k * n + i] * u[k * n + j];
                                        }
                                f = s / h;
                                for(k = i; k < m; ++k) {
                                        u[k * n + j] += f * u[k * n + i];
                                        }
                                }
parray("First loop u =",u,n,m);
                        }
                w[i] = g;
                s = 0.0;
                for (j = l; j < n; ++j) {
                        s += u[i * n + j] * u[i * n + j];
                        }
                if (s < tol) {
                        g = 0.0;
                        }
                else {
                        f = u[i * n + i + 1];
                        g = (f < 0.0) ? sqrt(s) : -sqrt(s);
                        h = f * g - s;
                        u[i * n + i + 1] = f - g;
                        for (j = l; j < n; ++j) {
                                temp[j] = u[i * n + j] / h;
                                }
                        for (j = l; j < m; ++j) {
                                s = 0.0;
                                for (k = l; k < n; ++k) {
                                        s += u[j * n + k] * u[i*n+k];
                                        }
                                for (k = l; k < n; ++k) {
                                        u[j*n+k] += s * temp[k];
                                        }
                                }
parray("Second loop u = ",u,n,m);
                        }
                y = fabs(w[i]) + fabs(temp[i]);
                if (y > x) {
                        x = y;
                        }
                }
parray("after bidiag u =",u,n,m);
/*
*       Now accumulate right-hand transforms if we are building v too.
*/
        if (flags & WITHV) for (i = n - 1; i >= 0; --i) {
                if (g != 0.0) {
                        h = u[i * n + i + 1] * g;
                        for (j = l; j < n; ++j) {
                                v[j * n + i] = u[i * n + j] / h;
                                }
                        for (j = l; j < n; ++j) {
                                s = 0.0;
                                for (k = l; k < n; ++k) {
                                        s += u[i * n + k] * v[k * n + j];
                                        }
                                for (k = l; k < n; ++k) {
                                        v[k * n + j] += s * v[k * n + i];
                                        }
                                }
                        }
                for (j = l; j < n; ++j) {
                        v[i * n + j] = (v[j * n + i] = 0.0);
                        }
                v[i * n + i] = 1.0;
                g = temp[i];
                l = i;
                }
parray("Computed v =",v,n,m);
/*
*       Now accumulate the left-hand transforms.
*/
        if (flags & WITHU) for (i = n - 1; i >= 0; --i) {
                l = i + 1;
                g = w[i];
                for (j = l; j < n; ++j) {
                        u[i * n + j] = 0.0;
                        }
                if (g != 0.0) {
                        h = u[i * n + i] * g;
                        for (j = l; j < n; ++j) {
                                s = 0.0;
                                for (k = l; k < m; ++k) {
                                        s += u[k * n + i] * u[k * n + j];
                                        }
                                f = s / h;
                                for (k = i; k < m; ++k) {
                                        u[k * n + j] += f * u[k * n + i];
                                        }
                                }
                        for (j = i; j < m; ++j) {
                                u[j * n + i] /= g;
                                }
                        }
                else {
                        for (j = i; j < m; ++j) {
                                u[j * n + i] = 0.0;
                                }
                        }
                u[i * n + i] += 1.0;
                }
parray("Computed u =",u,n,m);
/*
*       Now diagonalise the bidiagonal form. BEWARE GOTO's IN THE LOOP!!
*/
        eps = eps * x;
        for (k = n - 1; k >= 0; --k) {
testsplitting:
                for (l = k; l >= 0; --l) {
                        if (fabs(temp[l]) <= eps) 
                                goto testconvergence;
                        if (fabs(w[l - 1]) <= eps)
                                goto cancellation;
                        }
/*
*               Cancellation of temp[l] if l > 0;
*/
cancellation:
                c = 0.0;
                s = 1.0;
                l1 = l - 1;
                for (i = l; i <= k; ++i) {
                        f = s * temp[i];
                        temp[i] *= c;
                        if (fabs(f) <= eps) 
                                goto testconvergence;
                        g = w[i];
                        h = (w[i] = sqrt(f * f + g * g));
                        c = g/h;
                        s = -f/h;
                        if (flags & WITHU) for (j = 0; j < m; ++j) {
                                y = u[j * n + l1];
                                z = u[j * n + i];
                                u[j * n + l1] = y * c + z * s;
                                u[j * n + i] = -y * s + z * c;
                                }
                        }
testconvergence:

parray("at test conv u =",u,n,m);
parray("w = ",w,1,m);
parray("v =",v,n,m);

/* headas_chat(5,"%.2f\n",k); */
                z = w[k];
                if (l == k) goto convergence;
/*
*               Shift from bottom 2x2 minor.
*/
                x = w[l];
                y = w[k - 1];
                g = temp[k - 1];
                h = temp[k];
                f = ((y - z)*(y + z) + (g - h)*(g + h)) / (2 * h * y);
                g = sqrt(f * f + 1);
                f = ((x - z)*(x + z) + h*(y/((f < 0.0)?f-g:f+g) - h)) / x;

		/* headas_chat(5,"%.2f,%.2f,%.2f,%.2f,%.2f\n",x,y,g,h,f); */
/*
*               Next QR transformation.
*/
                c = (s = 1);
                for (i = l + 1; i <= k; ++i) {
                        g = temp[i];
                        y = w[i];
                        h = s * g;
                        g *= c;
                        temp[i - 1] = (z = sqrt(f * f + h * h));
                        c = f / z;
                        s = h/z;
                        f = x * c + g * s;
                        g = -x * s + g * c;
                        h = y * s;
                        y *= c;
                        if (flags & WITHV) for (j = 0; j < n; ++j) {
                                x = v[j * n + i - 1];
                                z = v[j * n + i];
                                v[j * n + i - 1] = x * c + z * s;
                                v[j * n + i] = -x * s + z * c;
                                }
                        w[i - 1] = (z = sqrt(f * f + h * h));
                        c = f / z;
                        s = h / z;
                        f = c * g + s * y;
                        x = -s * g + c * y;
                        if (flags & WITHU) for (j = 0 ; j < m; ++j) {
                                y = u[j * n + i - 1];
                                z = u[j * n + i];
                                u[j * n + i - 1] = y * c + z * s;
                                u[j * n + i] = -y * s + z * c;
                                }
                        }
                temp[l] = 0.0;
                temp[k] = f;
                w[k] = x;
                goto testsplitting;

convergence:
                if (z < 0.0) {
/*
*                       w[k] is made non-negative.
*/
                        w[k] = -z;
                        if (flags & WITHV) for (j = 0; j < n; ++j) {
                                v[j * n + k] = -v[j * n + k];
                                }
                        }
                }
 
       return(0);

        }

/* The calls to parray were just debugging calls to a routine that could print
the contents of an array in a form that a human could read. I have had no
problems with this code.
Hope that helps.
Brian. */

/*-------------------------------------------------------------*/

void parray (
    char *printstr,
    double *array,
    int n,
    int m)

     /* m,                          Number of rows 
        n,                          Number of columns (n <= m) */

{
    int i,j,nmax = 2,mmax=2;
    int number=6;

    if (n < nmax) nmax = n;
    if (m < nmax) mmax = m;

    headas_chat(number," %s\n",printstr);
    
    for (i=0;i<mmax;i++) {
       for (j=0;j<nmax;j++) {
	 headas_chat(number,"%.2f ",array[i*nmax+j]);
       }    
       headas_chat(number,"\n");
    }

}
