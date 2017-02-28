**==ycapdelim.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Capture a delimiter
*
      SUBROUTINE YCAPDELIM(Param)
 
      IMPLICIT NONE
      INCLUDE 'yaccfor.inc'
 
      CHARACTER*(MAXLINESIZE) Param
 
 
C      str1 = Sval(Npars-1)
C      n = Npars - 1
C      IF ( str1(LENACT(str1):LENACT(str1)).EQ.',' ) THEN
C         IF ( Npars.GE.2 ) THEN
C            str1 = Sval(Npars-2)
C           n = Npars - 2
C        ENDIF
C     ENDIF
C      Sval(n) = str1(1:LENACT(str1)) // ','
 
      RETURN
      END
