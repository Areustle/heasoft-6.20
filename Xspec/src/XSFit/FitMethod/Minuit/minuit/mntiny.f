*
* $Id: mntiny.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mntiny.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:30  dorman
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
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
 
      SUBROUTINE MNTINY(EPSP1,EPSBAK)
      INCLUDE "d506dp.inc"
CC        Compares its argument with the value 1.0, and returns
CC        the value .TRUE. if they are equal.  To find EPSMAC
CC        safely by foiling the Fortran optimizer
CC
      PARAMETER (ONE=1.0)
      EPSBAK =  EPSP1  - ONE
      RETURN
      END
