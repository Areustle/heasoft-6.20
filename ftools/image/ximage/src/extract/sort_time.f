      SUBROUTINE SORT_TIME
      implicit none
 
      character(80) cstr
      INTEGER*4 ierr
      INTEGER*4 LENACT
      cstr = 'sort -T . -o extract.tmp extract.tmp'
 
      CALL XWRITE('Sorting light curve',5)
      CALL SPAWN(cstr,LENACT(cstr),ierr)
      RETURN
      END
