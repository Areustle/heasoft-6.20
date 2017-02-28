*
* $Id: mnstat.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnstat.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:29  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:17  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:57  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
 
      SUBROUTINE MNSTAT(FMIN,FEDM,ERRDEF,NPARI,NPARX,ISTAT)
      INCLUDE "d506dp.inc"
CC       User-called
CC       Provides the user with information concerning the current status
CC          of the current minimization. Namely, it returns:
CC        FMIN: the best function value found so far
CC        FEDM: the estimated vertical distance remaining to minimum
CC        ERRDEF: the value of UP defining parameter uncertainties
CC        NPARI: the number of currently variable parameters
CC        NPARX: the highest (external) parameter number defined by user
CC        ISTAT: a status integer indicating how good is the covariance
CC           matrix:  0= not calculated at all
CC                    1= approximation only, not accurate
CC                    2= full matrix, but forced positive-definite
CC                    3= full accurate covariance matrix
CC
      INCLUDE "d506cm.inc"
      FMIN = AMIN
      FEDM = EDM
      ERRDEF = UP
      NPARI = NPAR
      NPARX = NU
      ISTAT = ISW(2)
        IF (EDM  .EQ. BIGEDM)  THEN
            FEDM = UP
        ENDIF
        IF (AMIN .EQ. UNDEFI)  THEN
            FMIN = 0.0
            FEDM = UP
            ISTAT= 0
        ENDIF
      RETURN
      END
