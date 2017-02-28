**==sort_time.spg  processed by SPAG 4.50F  at 12:47 on 21 Sep 1994
      SUBROUTINE SORT_TIME(fname)
 
      IMPLICIT NONE

      CHARACTER*(*) fname

      character(80) cstr
      INTEGER ierr
      INTEGER LENACT
 
      CALL MAKEFNAME(fname)
 
      cstr = 'sort -o '//fname(1:LENACT(fname))//' -T . '//fname
 
      CALL XWRITE('Sorting light curve',5)
      CALL SPAWN(cstr,LENACT(cstr),ierr)
      RETURN
      END
