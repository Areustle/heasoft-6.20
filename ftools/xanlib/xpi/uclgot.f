**==uclgot.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UCLGOT(Parname,Status)
 
*
* Tell if the parameter Parname has been entered on the command line
*
      IMPLICIT NONE
      CHARACTER*(*) Parname
      INTEGER*4 Status
 
      INTEGER*4 TBLFPR
      character(80) str1 , str2
 
      INCLUDE 'tbl.inc'
      INCLUDE 'yaccfor.inc'
 
      INTEGER*4 i
      INTEGER*4 j
 
      IF ( Status.NE.0 ) RETURN
 
 
      i = TBLFPR(Parname)
*
 
      IF ( i.EQ.0 ) THEN
         Status = 1
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 1
         RETURN
      ENDIF
 
 
      str1 = Parname
      CALL UPC(str1)
 
      DO 100 j = 1 , NPArs
         str2 = SPArs(j)
         CALL UPC(str2)
 
         IF ( str1.EQ.str2 ) THEN
            Status = 0
*            print*,'Got parameter ',parname(:LENACT(parname)),
*     &           ' from command line'
            RETURN
         ENDIF
 100  CONTINUE
 
      Status = -1
      RETURN
      END
 
 
 
