*
* $Id: mnvers.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnvers.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:30  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:18  dorman
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
 
      SUBROUTINE MNVERS(CV)
      INCLUDE "d506dp.inc"
CC         Returns the Minuit version in CV, char*6
CC
      INCLUDE "d506cm.inc"
      CHARACTER*(*) CV
      CV = CVRSN
      RETURN
      END
