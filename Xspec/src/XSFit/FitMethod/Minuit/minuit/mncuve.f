*
* $Id: mncuve.f,v 1.1.1.1 2003/11/25 22:45:44 dorman Exp $
*
* $Log: mncuve.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:44  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:17  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:08  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:53  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
 
      SUBROUTINE MNCUVE(FCN,FUTIL)
      INCLUDE "d506dp.inc"
CC        Makes sure that the current point is a local
CC        minimum and that the error matrix exists,
CC        or at least something good enough for MINOS and MNCONT
CC
      INCLUDE "d506cm.inc"
      EXTERNAL FCN,FUTIL
      IF (ISW(4) .LT. 1) THEN
          WRITE (ISYSWR,'(/A,A)')
     +    ' FUNCTION MUST BE MINIMIZED BEFORE CALLING ',CFROM
          APSI = EPSI
          CALL MNMIGR(FCN,FUTIL)
      ENDIF
      IF (ISW(2) .LT. 3)  THEN
         CALL MNHESS(FCN,FUTIL)
         IF (ISW(2) .LT. 1)  THEN
            CALL MNWARN('W',CFROM,'NO ERROR MATRIX.  WILL IMPROVISE.')
            DO 555 I=1,NPAR
              NDEX = I*(I-1)/2
              DO 554 J=1,I-1
              NDEX = NDEX + 1
  554         VHMAT(NDEX) = 0.
            NDEX = NDEX + 1
            IF (G2(I) .LE. ZERO)  THEN
              WINT = WERR(I)
              IEXT = NEXOFI(I)
              IF (NVARL(IEXT) .GT. 1) THEN
                 CALL MNDXDI(X(I),I,DXDI)
                 IF (ABS(DXDI) .LT. .001) THEN
                    WINT = .01
                 ELSE
                    WINT = WINT/ABS(DXDI)
                 ENDIF
              ENDIF
              G2(I) = UP/WINT**2
            ENDIF
            VHMAT(NDEX) = 2./G2(I)
  555       CONTINUE
            ISW(2) = 1
            DCOVAR = 1.
         ELSE
           CALL MNWERR
         ENDIF
      ENDIF
      RETURN
      END
