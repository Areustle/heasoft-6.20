*
* $Id: mndxdi.f,v 1.1.1.1 2003/11/25 22:45:44 dorman Exp $
*
* $Log: mndxdi.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:44  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:18  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:08  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:54  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
 
      SUBROUTINE MNDXDI(PINT,IPAR,DXDI)
      INCLUDE "d506dp.inc"
CC        calculates the transformation factor between external and
CC        internal parameter values.     this factor is one for
CC        parameters which are not limited.     called from MNEMAT.
      INCLUDE "d506cm.inc"
      I = NEXOFI(IPAR)
      DXDI = 1.0
      IF (NVARL(I) .GT. 1)
     +      DXDI = 0.5 *ABS((BLIM(I)-ALIM(I)) * COS(PINT))
      RETURN
      END
