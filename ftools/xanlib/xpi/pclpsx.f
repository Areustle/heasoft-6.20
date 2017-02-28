**==pclpsx.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE PCLPSX(Parname,Buffer,Status)
 
      CHARACTER*(*) Parname
      COMPLEX Buffer
      INTEGER*4 Status
 
      CALL UCLPSX(Parname,Buffer,Status)
      RETURN
      END
 
