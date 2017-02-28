**==pclgst.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLGST(Parname,Buffer,Status)
 
      CHARACTER*(*) Parname , Buffer
      INTEGER*4 Status
 
      CALL UCLGST(Parname,Buffer,Status)
      RETURN
      END
 
