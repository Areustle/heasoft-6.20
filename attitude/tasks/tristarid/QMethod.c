/*
 * $Source: /headas/headas/attitude/tasks/tristarid/QMethod.c,v $
 * $Revision: 1.7 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: QMethod.c,v $
 * Revision 1.7  2010/07/26 04:38:00  rwiegand
 * The tool now keeps track of two sorts of ambiguous matches: those which
 * are slightly better or worse in terms of distance relative to the primary
 * group, and those which do not yield a unique mapping between observed and
 * reference objects.  During the match filtering process, the hope is that
 * some ambiguities will resolve themselves allowing unambiguous matches to
 * be identified.
 *
 * Revision 1.6  2006/02/22 15:50:56  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.5  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.4  2005/08/27 12:52:09  wiegand
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
#include <stdio.h>
#include <stdlib.h>

#include "report.h"

#include "EigenvalueDecomposition.h"
#include "Match.h"
#include "QMethod.h"
#include "MUtil.h"


int QMethod_solve (List * matches, double q[4])
{
   edPtr decomp;
   int nMatches;
   double B[3][3] = { {0, 0, 0},
                      {0, 0, 0},
                      {0, 0, 0} };
   double wi[3];
   double vi[3];
   double outer[3][3], S[3][3];
   double Zt[3];
   double K[4][4];

   double sigma, totalWeight = 0;

   int i;
   int best = -1;

   nMatches = List_size(matches);

   if (nMatches < 2) {
      report_error("QMethod.solve: requires at least 2 matches\n");
      return 1;
   }

   for (i = 0; i < nMatches; i++) {
      Match *m = List_get(matches, i);
      totalWeight += m->weight;
   }


   for (i = 0; i < nMatches; i++) {
      Match *m = List_get(matches, i);
      double weight = sqrt(m->weight / totalWeight);

      Math_v3scale(m->obs->unit, weight, wi);
      Math_v3scale(m->ref->unit, weight, vi);

      Math_v3outerx(wi, vi, outer);

      Math_minc(B, outer);
   }

   Zt[0] = B[1][2] - B[2][1];
   Zt[1] = B[2][0] - B[0][2];
   Zt[2] = B[0][1] - B[1][0];

   sigma = B[0][0] + B[1][1] + B[2][2];

   S[0][0] = B[0][0] + B[0][0];
   S[0][1] = B[0][1] + B[1][0];
   S[0][2] = B[0][2] + B[2][0];
   S[1][0] = B[1][0] + B[0][1];
   S[1][1] = B[1][1] + B[1][1];
   S[1][2] = B[1][2] + B[2][1];
   S[2][0] = B[2][0] + B[0][2];
   S[2][1] = B[2][1] + B[1][2];
   S[2][2] = B[2][2] + B[2][2];

   K[0][0] = S[0][0] - sigma;
   K[0][1] = S[0][1];
   K[0][2] = S[0][2];
   K[0][3] = Zt[0];
   K[1][0] = S[1][0];
   K[1][1] = S[1][1] - sigma;
   K[1][2] = S[1][2];
   K[1][3] = Zt[1];
   K[2][0] = S[2][0];
   K[2][1] = S[2][1];
   K[2][2] = S[2][2] - sigma;
   K[2][3] = Zt[2];
   K[3][0] = Zt[0];
   K[3][1] = Zt[1];
   K[3][2] = Zt[2];
   K[3][3] = sigma;

   decomp = (edPtr) malloc( sizeof( EigenvalueDecomposition) );
   EigenvalueDecomposition_construct(decomp, K);

   for (i = 0; i < decomp->n; ++i)
      if (decomp->e[i] == 0)
         if (best < 0 || decomp->d[i] > decomp->d[best])
            best = i;

   if (best < 0) {
      printf("no real eigenvalues\n");
      return 2;
   }

   for (i = 0; i < 4; i++)
      q[i] = decomp->V[i][best];

   Math_qNormalize(q);

   free(decomp);
   decomp = 0;

   return 0;
}

