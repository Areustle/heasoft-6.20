*
* $Id: mncler.f,v 1.1.1.1 2003/11/25 22:45:44 dorman Exp $
*
* $Log: mncler.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:44  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:16  dorman
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
 
      SUBROUTINE MNCLER
      INCLUDE "d506dp.inc"
CC        Called from MINUIT and by option from MNEXCM
CC        Resets the parameter list to UNDEFINED
      INCLUDE "d506cm.inc"
      NPFIX = 0
      NU = 0
      NPAR = 0
      NFCN = 0
      NWRMES(1) = 0
      NWRMES(2) = 0
      DO 10 I= 1, MAXEXT
      U(I) = 0.0
      CPNAM(I) = CUNDEF
      NVARL(I) = -1
   10 NIOFEX(I) = 0
      CALL MNRSET(1)
      CFROM = 'CLEAR   '
      NFCNFR = NFCN
      CSTATU ='UNDEFINED '
      LNOLIM = .TRUE.
      LPHEAD = .TRUE.
      RETURN
      END
