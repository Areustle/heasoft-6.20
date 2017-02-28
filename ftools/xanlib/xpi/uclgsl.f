**==uclgsl.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UCLGSL(Parname,Buffer,Status)
 
*
* return a integer*8 value related to parname
*
      IMPLICIT NONE
      CHARACTER*(*) Parname
      REAL*8 Buffer
      INTEGER*4 Status
* this isn't supported
 
      Status = 2
      RETURN
      END
