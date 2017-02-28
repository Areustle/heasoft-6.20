**==xpiparmode.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE XPIPARMODE(Name,New,Old,Status)
 
* sets parameter 'name' to new mode, returns old mode in old
 
      CHARACTER*(*) Name , New , Old
      INTEGER*4 Status , i , TBLFPR
 
      INCLUDE 'tbl.inc'
 
 
      IF ( Status.NE.0 ) RETURN
 
      i = TBLFPR(Name)
 
      IF ( i.EQ.0 ) THEN
         Status = 1
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 1
         RETURN
      ENDIF
 
      Old = TBLpupd(i)
      TBLpupd(i) = New
      RETURN
      END
