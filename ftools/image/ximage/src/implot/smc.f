      SUBROUTINE SMC(Cmdid, Status)
      IMPLICIT NONE
c
c  Set the minimum color level for disp     3/29/94
c
c  I  cmdid   (i)  Command id
c  O  status  (i)  Error flag (0=OK)
c
      integer cmdid, status

      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 min_level, argc
      LOGICAL isdisplay
      character(20) string
      real*8 dd

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.1 ) then
         call xwrite(' Usage: smc <minimum color level>', 10)
         status = -1
      endif
      call nextcarg(cmdid,string, 20, status)
      if ( status.ne.0 ) return

      call strnum(string, -4, dd, status)
      if ( status.ne.0 ) return
      min_level = int(dd)
 
      if ( .not.isdisplay() ) then
         call xwrite(' No display', 10)
         status = -1
         return
      endif

      call refresh_coltab
      CALL PGFILLBG(min_level)
c
      RETURN
      END
