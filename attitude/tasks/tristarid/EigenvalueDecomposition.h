/*
 * $Source: /headas/headas/attitude/tasks/tristarid/EigenvalueDecomposition.h,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:50:58 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: EigenvalueDecomposition.h,v $
 * Revision 1.4  2005/08/27 12:50:58  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef EIGENVALUEDECOMPOSITION_H
#define EIGENVALUEDECOMPOSITION_H

typedef struct
{
   /* Dimention */
   int n;

   int issymmetric;

   /* Eigenvalues */
   double d[4];
   double e[4];

   /* Eigenvectors */
   double V[4][4];

   /* Hessenburg form */
   double H[4][4];

   /* working storage */
   double ort[4];

   double cdivr;
   double cdivi;

} EigenvalueDecomposition;

typedef EigenvalueDecomposition *edPtr;

/* Symmetric Householder reduction to tridiagonal form */
void EigenvalueDecomposition_tred2(edPtr eVD);

/* Symmetric tridiagonal QL algorithm. */
void EigenvalueDecomposition_tql2 (edPtr eVD);

/* Nonsymmetric reduction to Hessenberg form. */
void EigenvalueDecomposition_orthes (edPtr eVD);

/* Complex scalar division. */
void EigenvalueDecomposition_cdiv(edPtr eVD, double xr, double xi, double yr, double yi); 

/* Nonsymmetric reduction from Hessenberg to real Schur form. */
void EigenvalueDecomposition_hqr2 (edPtr eVD);

void EigenvalueDecomposition_construct (edPtr eVD, double A[4][4]);

#endif
