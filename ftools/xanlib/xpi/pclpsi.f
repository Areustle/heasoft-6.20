**==pclpsi.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLPSI(Parname,Buffer,Status)
 
      CHARACTER*(*) Parname
      INTEGER*4 Buffer , Status
 
      CALL UCLPSI(Parname,Buffer,Status)
      RETURN
      END
 
