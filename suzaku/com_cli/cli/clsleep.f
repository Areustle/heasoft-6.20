C
C File: clsleep.f
C Description: Sleep for the specified seconds
C
C Public:  CLSLEEP
C
C History:
C     16-Feb-2005, Y.ISHISAKI   CLSLEEP Created
C
      Subroutine CLsleep( DSEC )
      Implicit None
C input
      Real * 8  DSEC
C local
      Integer SEC, USEC
C begin
      If ( DSEC .le. 0.D0 ) Return
C
      SEC = INT( DSEC )
      USEC = INT( (DSEC - REAL(SEC)) * 1.D6 )
      If ( SEC .gt. 0 ) Call cli__Fsleep(SEC)
      If ( USEC .gt. 0 ) Call cli__Fusleep(USEC)
C
      Return
      End
