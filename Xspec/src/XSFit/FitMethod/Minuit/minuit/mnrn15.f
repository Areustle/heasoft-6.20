*
* $Id: mnrn15.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnrn15.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:27  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:15  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:56  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
 
      SUBROUTINE MNRN15(VAL,INSEED)
      INCLUDE "d506dp.inc"
C         This is a super-portable random number generator.
C         It should not overflow on any 32-bit machine.
C         The cycle is only ~10**9, so use with care!
C         Note especially that VAL must not be undefined on input.
C                    Set Default Starting Seed
      PARAMETER (THREE=3.0)
      DATA ISEED/12345/
      IF (VAL .EQ. THREE)  GO TO 100
C
      INSEED = ISEED
      K = ISEED/53668
      ISEED = 40014*(ISEED-K*53668) - K*12211
      IF (ISEED .LT. 0) ISEED = ISEED + 2147483563
      VAL = REAL(ISEED) * 4.656613E-10
      RETURN
C               "entry" to set seed, flag is VAL=3.
  100 ISEED = INSEED
      RETURN
      END
