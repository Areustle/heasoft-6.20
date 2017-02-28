
      SUBROUTINE gtdellog(progname)
      character*(*) progname
      character(100) ctmp
      INTEGER*4 LENACT
      INTEGER*4 ierr
 
      ctmp = 'rm -f '//progname
 
      CALL SPAWN(ctmp,LENACT(ctmp),ierr)

      RETURN
      END
