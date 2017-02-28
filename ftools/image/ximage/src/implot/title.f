      subroutine title(Cmdid,Uptitle, Lotitle,Status)
      implicit none
c
c change c/d top title for image display
c
c  I  Cmdid      (i)  Command id
c  O  Uptitle    (c)  text immediately above Lotitle
c  O  Lotitle    (c)  text immediately above viewport
c  O  Status     (i)  Error flag (0=OK)
c
      INTEGER Cmdid, Status
      character(80) Uptitle, Lotitle
c
c  Local variables
c
      INTEGER argc
      character(80) answer
      INCLUDE '../include/io.inc'
c
c is it the lower or upper title? Use the upper title by default
c
      LOGICAL lower, reset
c
c initialize variable
c
      lower = .FALSE.
      reset = .FALSE.

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
         CALL GPARL(Cmdid,'LOWER',lower,status)
         call nextcarg(cmdid,answer, 80, status)
      else
         CALL GPARL(Cmdid,'RESET',reset,status)
         if ( .not.reset ) call wrongargs(cmdid, status)
      endif

      if ( status.ne.0 ) return
c
      answer(80:80) = '*'
 
      IF ( lower ) THEN
         Lotitle = answer
      ELSE
         Uptitle = answer
      ENDIF

      if ( reset ) then
         Lotitle = ' '
         Uptitle = ' '
      endif
 
      RETURN
      END
