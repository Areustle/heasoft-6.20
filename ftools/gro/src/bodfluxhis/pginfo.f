c       --------------------------------------------------
              SUBROUTINE PGINFO
C-----------------------------------------------------------------------
C This subroutine tests PGQINF and displays the information returned on
C the standard output.
C-----------------------------------------------------------------------
      character(64) VALUE
      INTEGER LENGTH
      REAL X, Y, X1, X2, Y1, Y2
C
C Information available from PGQINF:
C
      CALL PGQINF('version',  VALUE, LENGTH)
      WRITE (*,*) 'version=', VALUE(:LENGTH)
      CALL PGQINF('state',    VALUE, LENGTH)
      WRITE (*,*) 'state=',   VALUE(:LENGTH)
      CALL PGQINF('user',     VALUE, LENGTH)
      WRITE (*,*) 'user=',    VALUE(:LENGTH)
      CALL PGQINF('now',      VALUE, LENGTH)
      WRITE (*,*) 'now=',     VALUE(:LENGTH)
      CALL PGQINF('device',   VALUE, LENGTH)
      WRITE (*,*) 'device=',  VALUE(:LENGTH)
      CALL PGQINF('file',     VALUE, LENGTH)
      WRITE (*,*) 'file=',    VALUE(:LENGTH)
      CALL PGQINF('type',     VALUE, LENGTH)
      WRITE (*,*) 'type=',    VALUE(:LENGTH)
      CALL PGQINF('dev/type', VALUE, LENGTH)
      WRITE (*,*) 'dev/type=',VALUE(:LENGTH)
      CALL PGQINF('hardcopy', VALUE, LENGTH)
      WRITE (*,*) 'hardcopy=',VALUE(:LENGTH)
      CALL PGQINF('terminal', VALUE, LENGTH)
      WRITE (*,*) 'terminal=',VALUE(:LENGTH)
      CALL PGQINF('cursor',   VALUE, LENGTH)
      WRITE (*,*) 'cursor=',  VALUE(:LENGTH)
C
C Get view surface dimensions:
C
      CALL PGQVSZ(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
      WRITE (*,100) X, Y, X*25.4, Y*25.4
  100 FORMAT (' Plot dimensions (x,y; inches): ',F9.2,', ',F9.2/
     1        '                          (mm): ',F9.2,', ',F9.2)
C-----------------------------------------------------------------------
      END