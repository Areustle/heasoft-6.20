**==pclpsr.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLPSR(Parname,Buffer,Status)
 
      REAL Buffer
      CHARACTER*(*) Parname
      INTEGER Status
 
 
      CALL UCLPSR(Parname,Buffer,Status)
      RETURN
      END
 
