**==pclgss.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLGSS(Parname,Buffer,Status)
 
      CHARACTER*(*) Parname
      INTEGER*2 Buffer
      INTEGER*4 Status
 
 
      CALL UCLGSS(Parname,Buffer,Status)
      RETURN
      END
 
