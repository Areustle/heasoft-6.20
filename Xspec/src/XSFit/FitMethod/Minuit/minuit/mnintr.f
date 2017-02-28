*
* $Id: mnintr.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnintr.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:22  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:12  dorman
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
 
      SUBROUTINE MNINTR(FCN,FUTIL)
      INCLUDE "d506dp.inc"
CC       Called by user. Interfaces to MNREAD to allow user to change
CC       easily from Fortran-callable to interactive mode.
CC
      INCLUDE "d506cm.inc"
      EXTERNAL FCN,FUTIL
      IFLGIN = 3
      CALL MNREAD(FCN,IFLGIN,IFLGUT,FUTIL)
      WRITE (ISYSWR,'(2A/)')  ' END OF MINUIT COMMAND INPUT. ',
     +      '   RETURN TO USER PROGRAM.'
      RETURN
      END
