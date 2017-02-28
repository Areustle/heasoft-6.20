**==pclgsg.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
 
      SUBROUTINE PCLGSG(Parname,Buffer1,N,Rmin,Rmax,Nr,Status)
 
      CHARACTER*(*) Parname
      INTEGER*4 N
      REAL*4 Buffer1(2,N) , Rmin , Rmax
      INTEGER*4 Nr
      INTEGER*4 Status
 
 
 
      CALL UCLGSG(Parname,Buffer1,N,Rmin,Rmax,Nr,Status)
 
 
      RETURN
      END
 
