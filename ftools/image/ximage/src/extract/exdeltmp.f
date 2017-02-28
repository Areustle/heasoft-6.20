      SUBROUTINE EXDELTMP
      implicit none
      character(100) ctmp
      INTEGER*4 LENACT
      INTEGER*4 ierr
 
      ctmp = 'rm -f extract.tmp'
 
      CALL SPAWN(ctmp,LENACT(ctmp),ierr)
      RETURN
      END
