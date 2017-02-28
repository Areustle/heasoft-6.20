#include <algorithm>

int svdcmp(double* mat, int m, int n, double* wvec, double* vmat, double* rv1);

void invertSVD(double* matrix, int n, bool qzero, double *wvec, double *vmat)
{
   //---
   // Function to perform a matrix inversion using singular value
   // decomposition to attempt to stabilize against numerical inaccuracies.
   // Translated from the Fortran xmatin.f.
   // This ASSUMES only that matrix is square.  It does not have
   // to be symmetrical.
   //---
   // matrix  I/   Square array to be inverted
   //             O  Inverse of array
   // N       I    Actual size of data in array
   // QZERO   I    If true then zero out principal axes whose weightings
   //              are < 1E-6 times the maximum weighting.
   // WVEC    R    Vector of variances on principal axes
   // VMAT    R    Array of principal axes
   //---

   // Do the SVD on the matrix:
   // matrix = UwV_T where matrix is replaced with U and
   // vmat is filled with V (not V_T).

   const int n2 = n*n;
   double* workspace = new double[n];

   svdcmp(matrix, n, n, wvec, vmat, workspace);

   delete [] workspace;

   // Find the largest singular value.
   double wmax = 0.0;
   for (int i=0; i<n; ++i)
      wmax = std::max(wmax,wvec[i]);
   double wmin = wmax * 1.0e-12;

   // Zero out the inverses of those vector elements below the minimum
   // defined above.
   for (int i=0; i<n; ++i)
   {
      if (qzero && wvec[i] < wmin)
         wvec[i] = 0.0;
      else if (wvec[i] != 0.0)
         wvec[i] = 1.0/wvec[i];
   }

   // Calculate the inverse.
   // invMatrix = V(1/w)U_T
   double* invMatrix = new double[n2];
   for (int k=0; k<n; ++k)
      for (int j=0; j<n; ++j)
      {
         int ielem = k*n+j;
         invMatrix[ielem] = 0.0;
         for (int i=0; i<n; ++i)
            invMatrix[ielem] += vmat[j*n+i]*wvec[i]*matrix[k*n+i];
      }

   // Load the inverse into the output array
   for (int i=0; i<n2; ++i)
      matrix[i] = invMatrix[i];

   delete [] invMatrix;
}
