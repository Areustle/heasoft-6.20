*
* $Id: mncalf.f,v 1.1.1.1 2003/11/25 22:45:44 dorman Exp $
*
* $Log: mncalf.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:44  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:15  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:06  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:53  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:28  mclareni
* Minuit
*
*
 
      SUBROUTINE MNCALF(FCN,PVEC,YCALF,FUTIL)
      INCLUDE "d506dp.inc"
CC        Called only from MNIMPR.  Transforms the function FCN
CC        by dividing out the quadratic part in order to find further
CC        minima.    Calculates  ycalf = (f-fmin)/(x-xmin)*v*(x-xmin)
CC
      INCLUDE "d506cm.inc"
      EXTERNAL FCN,FUTIL
      DIMENSION PVEC(15)
      NPARX = NPAR
      CALL MNINEX(PVEC)
      CALL FCN(NPARX,GIN,F,U,4,FUTIL)
      NFCN = NFCN + 1
      DO 200 I= 1, NPAR
      GRD(I) = 0.
         DO 200 J= 1, NPAR
         M = MAX(I,J)
         N = MIN(I,J)
         NDEX = M*(M-1)/2 + N
  200    GRD(I) = GRD(I) + VTHMAT(NDEX) * (XT(J)-PVEC(J))
      DENOM = 0.
      DO 210 I= 1, NPAR
  210 DENOM = DENOM + GRD(I) * (XT(I)-PVEC(I))
      IF (DENOM .LE. ZERO)  THEN
         DCOVAR = 1.
         ISW(2) = 0
         DENOM = 1.0
      ENDIF
      YCALF = (F-APSI) / DENOM
      RETURN
      END
