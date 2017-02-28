*
* $Id: mnseti.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnseti.f,v $
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
 
      SUBROUTINE MNSETI(TIT)
      INCLUDE "d506dp.inc"
CC       Called by user to set or change title of current task.
CC
      INCLUDE "d506cm.inc"
      CHARACTER*(*) TIT
      CTITL = TIT
      RETURN
      END
