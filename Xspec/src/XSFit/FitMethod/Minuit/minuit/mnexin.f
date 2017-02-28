*
* $Id: mnexin.f,v 1.1.1.1 2003/11/25 22:45:44 dorman Exp $
*
* $Log: mnexin.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:44  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:19  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:09  dorman
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
 
      SUBROUTINE MNEXIN(PINT)
      INCLUDE "d506dp.inc"
CC        Transforms the external parameter values U to internal
CC        values in the dense array PINT. Subroutine MNPINT is used.
CC
      INCLUDE "d506cm.inc"
      DIMENSION PINT(*)
      LIMSET = .FALSE.
      DO 100  IINT= 1, NPAR
      IEXT = NEXOFI(IINT)
      CALL MNPINT(U(IEXT),IEXT,PINTI)
      PINT(IINT) = PINTI
  100 CONTINUE
      RETURN
      END
