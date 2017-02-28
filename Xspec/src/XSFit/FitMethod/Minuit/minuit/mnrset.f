*
* $Id: mnrset.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnrset.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:28  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:15  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:56  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
 
      SUBROUTINE MNRSET(IOPT)
      INCLUDE "d506dp.inc"
CC        Called from MNCLER and whenever problem changes, for example
CC        after SET LIMITS, SET PARAM, CALL FCN 6
CC    If IOPT=1,
CC        Resets function value and errors to UNDEFINED
CC    If IOPT=0, sets only MINOS errors to undefined
      INCLUDE "d506cm.inc"
      CSTATU = 'RESET     '
      IF (IOPT .GE. 1)  THEN
        AMIN = UNDEFI
        FVAL3 = 2.0*ABS(AMIN) + 1.
        EDM = BIGEDM
        ISW(4) = 0
        ISW(2) = 0
        DCOVAR = 1.
        ISW(1) = 0
      ENDIF
      LNOLIM = .TRUE.
      DO 10 I= 1, NPAR
      IEXT = NEXOFI(I)
      IF (NVARL(IEXT) .GE. 4) LNOLIM=.FALSE.
      ERP(I) = ZERO
      ERN(I) = ZERO
      GLOBCC(I) = ZERO
   10 CONTINUE
      IF (ISW(2) .GE. 1)  THEN
         ISW(2) = 1
         DCOVAR = MAX(DCOVAR,HALF)
      ENDIF
      RETURN
      END
