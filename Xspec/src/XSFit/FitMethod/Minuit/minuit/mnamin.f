*
* $Id: mnamin.f,v 1.1.1.1 2003/11/25 22:45:44 dorman Exp $
*
* $Log: mnamin.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:44  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:15  dorman
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
 
      SUBROUTINE MNAMIN(FCN,FUTIL)
      INCLUDE "d506dp.inc"
CC        Called  from many places.  Initializes the value of AMIN by
CC        calling the user function. Prints out the function value and
CC        parameter values if Print Flag value is high enough.
CC
      INCLUDE "d506cm.inc"
      EXTERNAL FCN,FUTIL
      NPARX = NPAR
      IF (ISW(5) .GE. 1) WRITE (ISYSWR,'(/A,A)') ' FIRST CALL TO ',
     + 'USER FUNCTION AT NEW START POINT, WITH IFLAG=4.'
      CALL MNEXIN(X)
      CALL FCN(NPARX,GIN,FNEW,U,4,FUTIL)
      NFCN = NFCN + 1
      AMIN = FNEW
      EDM = BIGEDM
      RETURN
      END
