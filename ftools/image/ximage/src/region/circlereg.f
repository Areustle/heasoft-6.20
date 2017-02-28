      SUBROUTINE circlereg(cmdid, status)
      implicit none
c get the circle region
c
c I Cmdid    (i)  Command id
c O Status   (i)  status error 
c
      integer Cmdid, Status
c
c  Local variables
c
      INTEGER*4 regtype
c
       regtype=1 
       call getregions(cmdid, regtype, status)
       IF (status.NE.0) THEN 
          status=1
          goto 100
       ENDIF 
100    RETURN
       END    
