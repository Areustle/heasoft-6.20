**==XINDEX.spg  processed by SPAG 3.09I  at 09:48 on 20 Aug 1992
*- xindex - a rose by any other name
      INTEGER FUNCTION XINDEX(String,Sub_string)
* Description :
*  interface to FORTRAN index function for those annoying occasions when you
*  also want to use a variable called index
* Author :
*  Andy Pollock
*-
      CHARACTER*(*) String
      CHARACTER*(*) Sub_string
 
      XINDEX = INDEX(String,Sub_string)
 
      RETURN
 
      END
