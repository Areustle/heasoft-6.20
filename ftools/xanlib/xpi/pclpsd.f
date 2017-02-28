**==pclpsd.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLPSD(Parname,Buffer,Status)
 
      CHARACTER*(*) Parname
      REAL*8 Buffer
      INTEGER*4 Status
 
      CALL UCLPSD(Parname,Buffer,Status)
      RETURN
      END
 
