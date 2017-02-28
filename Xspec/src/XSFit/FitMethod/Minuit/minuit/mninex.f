*
* $Id: mninex.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mninex.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:22  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:11  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:55  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
 
      SUBROUTINE MNINEX(PINT)
      INCLUDE "d506dp.inc"
CC        Transforms from internal coordinates (PINT) to external
CC        parameters (U).   The minimizing routines which work in
CC        internal coordinates call this routine before calling FCN.
      INCLUDE "d506cm.inc"
      DIMENSION PINT(*)
      DO 100 J= 1, NPAR
      I = NEXOFI(J)
      IF (NVARL(I) .EQ. 1) THEN
         U(I) = PINT(J)
      ELSE
         U(I) = ALIM(I) + 0.5*(SIN(PINT(J)) +1.0) * (BLIM(I)-ALIM(I))
      ENDIF
  100 CONTINUE
      RETURN
      END
