**==pclgsb.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLGSB(Parname,Buffer,Status)
 
      CHARACTER*(*) Parname
      LOGICAL*4 Buffer
      INTEGER*4 Status
 
      CALL UCLGSB(Parname,Buffer,Status)
      RETURN
      END
 
