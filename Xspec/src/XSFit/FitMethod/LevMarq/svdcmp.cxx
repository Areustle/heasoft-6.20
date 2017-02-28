#include <XSUtil/FunctionUtils/xsFortran.h>
#include <cmath>
#include <algorithm>
#include <limits>

namespace {
   // Equivalent of the built-in Fortran SIGN function.
   double sign(const double x, const double y);
}

int svdcmp(double* mat, int m, int n, double* wvec, double* vmat, double* rv1)
{
   // Routine to do singular value decomposition of the matrix A.
   // Based on the Press etal routine

   int status=0;
   const double EPS = std::numeric_limits<double>::epsilon();
   
   // Create an array of pointers to input matrices to allow
   //   usage of '[][]' notation.
   double **a = new double*[m];
   double **v = new double*[n];   
   for (int i=0; i<m; ++i)
      a[i] = mat + i*n;
   for (int i=0; i<n; ++i)
      v[i] = vmat + i*n;
   double f = 0.0;
   double g = 0.0;
   double scale = 0.0;
   double anorm = 0.0;
   double s = 0.0;
   int l = 0;
   for (int i=0; i<n; ++i)
   {
      l = i + 1;
      rv1[i] = scale*g;
      g = 0.0;
      s = 0.0;
      scale = 0.0;
      if (i < m)
      {
         for (int k=i; k<m; ++k)
            scale += fabs(a[k][i]);
         if (scale != 0.0)
         {
            for (int k=i; k<m; ++k)
            {
               a[k][i] /= scale;
               s += a[k][i]*a[k][i];
            }
            f = a[i][i];
            g = -sign(sqrt(s), f);
            double h = f*g - s;
            a[i][i] = f - g;
            if (i != n-1)
            {
               for (int j=l; j<n; ++j)
               {
                  s = 0.0;
                  for (int k=i; k<m; ++k)
                     s += a[k][i]*a[k][j];
                  f = s/h;
                  for (int k=i; k<m; ++k)
                     a[k][j] += f*a[k][i];
               }               
            }
            for (int k=i; k<m; ++k)
               a[k][i] *= scale;
         } // end if scale != 0
      } // end if i < m
      wvec[i] = scale*g;
      g = 0.0;
      s = 0.0;
      scale = 0.0;
      if (i < m && i != n-1)
      {
         for (int k=l; k<n; ++k)
            scale += fabs(a[i][k]);
         if (scale != 0.0)
         {
            for (int k=l; k<n; ++k)
            {
               a[i][k] /= scale;
               s += a[i][k]*a[i][k];
            }
            f = a[i][l];
            g = -sign(sqrt(s), f);
            double h = f*g - s;
            a[i][l] = f - g;
            for (int k=l; k<n; ++k)
               rv1[k] = a[i][k]/h;
            if (i != m-1)
            {
               for (int j=l; j<m; ++j)
               {
                  s = 0.0;
                  for (int k=l; k<n; ++k)
                     s += a[j][k]*a[i][k];
                  for (int k=l; k<n; ++k)
                     a[j][k] += s*rv1[k];
               }
            } // end if i != m-1
            for (int k=l; k<n; ++k)
               a[i][k] *= scale;
         } // end if scale != 0
      } // end if i<m && i != n-1
      anorm = std::max(anorm, fabs(wvec[i])+fabs(rv1[i]));
   }
   
   for (int i=n-1; i>=0; --i)
   {
      if (i < n-1)
      {
         if (g != 0.0)
         {
            for (int j=l; j<n; ++j)
               v[j][i] = a[i][j]/a[i][l]/g;
            for (int j=l; j<n; ++j)
            {
               double s = 0.0;
               for (int k=l; k<n; ++k)
                  s += a[i][k]*v[k][j];
               for (int k=l; k<n; ++k)
                  v[k][j] += s*v[k][i];
            }
         }
         for (int j=l; j<n; ++j)
         {
            v[i][j] = 0.0;
            v[j][i] = 0.0;
         }
      } // end if i < n-1
      v[i][i] = 1.0;
      g = rv1[i];
      l = i;
   }
   
   for (int i=n-1; i>=0; --i)
   {
      l = i + 1;
      g = wvec[i];
      if (i < n-1)
         for (int j=l; j<n; ++j)
            a[i][j] = 0.0;
      if (g != 0.0)
      {
         g = 1.0/g;
         if (i != n-1)
         {
            for (int j=l; j<n; ++j)
            {
               double s = 0.0;
               for (int k=l; k<m; ++k)
                  s += a[k][i]*a[k][j];
               f = (s/a[i][i])*g;
               for (int k=i; k<m; ++k)
                  a[k][j] += f*a[k][i];
            }
         }
         for (int j=i; j<m; ++j)
            a[j][i] *= g;
      }
      else
      {
         for (int j=i; j<m; ++j)
            a[j][i] = 0.0;
      }
      a[i][i] += 1.0;
   }
   
   for (int k=n-1; k>=0; --k)
   {
      for (int its=1; its<=30; ++its)
      {
         int l1=0;
         double c=0.0;
         double x=0.0;
         int nm=0;
         bool isRv1Zero=false;
         bool isWvecZero=false;
         for (l=k; l>=0; --l)
         {
            l1 = l;
            // Note that rv1[0] is always 0, so nm = -1 shouldn't
            // reach the 'if' block following this 'for' loop.
            nm = l - 1;
            if (fabs(rv1[l]) <= EPS*anorm)
            {
               isRv1Zero = true;
               break;
            }
            if (fabs(wvec[nm]) <= EPS*anorm)
            {
               isWvecZero = true;
               break;
            }
         }
         if (!isRv1Zero)
         {
            l = l1;
            c = 0.0;
            s = 1.0;
            
            if (nm == -1)
            {
               status = -1;
               char err[]="Error: problems in SVDCMP likely due to NaN input (1)";
               xs_write(err, 2);
               delete [] a;
               delete [] v;
               return status;
            }
            
            for (int i=l; i<=k; ++i)
            {
               f = s*rv1[i];
               rv1[i] *= c;
               if (fabs(f) > EPS*anorm)
               {
                  g = wvec[i];
                  double h = sqrt(f*f+g*g);
                  wvec[i] = h;
                  h = 1.0/h;
                  c = g*h;
                  s = -f*h;
                  for (int j=0; j<m; ++j)
                  {
                     double y = a[j][nm];
                     double z = a[j][i];
                     a[j][nm] = y*c + z*s;
                     a[j][i] = -y*s + z*c;
                  }
               }
            }
         }
         double z = wvec[k];
         if (l == k)
         {
            if (z < 0.0)
            {
               wvec[k] = -z;
               for (int j=0; j<n; ++j)
                  v[j][k] = -v[j][k];
            }
            // Must exit iterations loop.
            break;
         }
         else
         {
            if (its == 30)
            {
               char warn[]="SVDCMP: No convergence in 30 iterations";
               xs_write(warn, 10);
            }
            x = wvec[l];
            nm = k - 1;
            if (nm < 0)
            {
               status = -1;
               char err[]="Error: problems in SVDCMP likely due to NaN input (2)";
               xs_write(err, 2);
               delete [] a;
               delete [] v;
               return status;               
            }
            double y = wvec[nm];
            g = rv1[nm];
            double h = rv1[k];
            f = ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
            g = sqrt(f*f+1.0);
            f = ((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x;
            c = 1.0;
            s = 1.0;
            for (int j=l; j<=nm; ++j)
            {
               int i = j + 1;
               g = rv1[i];
               y = wvec[i];
               h = s*g;
               g = c*g;
               z = sqrt(f*f+h*h);
               rv1[j] = z;
               c = f/z;
               s = h/z;
               f = (x*c) + (g*s);
               g = -(x*s) + (g*c);
               h = y*s;
               y = y*c;
               for (int jj=0; jj<n; ++jj)
               {
                  x = v[jj][j];
                  z = v[jj][i];
                  v[jj][j] = (x*c) + (z*s);
                  v[jj][i] = -(x*s) + (z*c);
               }
               z = sqrt(f*f+h*h);
               wvec[j] = z;
               if (z != 0.0)
               {
                  z = 1.0/z;
                  c = f*z;
                  s = h*z;
               }
               f = (c*g) + (s*y);
               x = -(s*g) + (c*y);
               for (int jj=0; jj<m; ++jj)
               {
                  y = a[jj][j];
                  z = a[jj][i];
                  a[jj][j] = (y*c) + (z*s);
                  a[jj][i] = -(y*s) + (z*c);
               }
            } // end loop over j
         } // end if l != k
         rv1[l] = 0.0;
         rv1[k] = f;
         wvec[k] = x;
      } // end iterations loop
      
   } // end loop over k
   
   delete [] a;
   delete [] v;
   return status;
}

namespace {

double sign(const double x, const double y)
{
   double val = fabs(x);
   if (y < 0.0)
      val *= -1.0;
   return val;
}

} // end unnamed namespace
