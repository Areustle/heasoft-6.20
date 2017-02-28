      SUBROUTINE shfdoc
C---
C DACHE subroutine that WRITEs some useful information to the
C default output unit
C---
C 24-Jul-1985 - rashafer
C---
C	WRITE(*,*)'    XHELP - a Standard Help Facility:  Brief notes:'
      WRITE (*, *) 'At any prompt you can type the following:'
      WRITE (*, *) '<CTRL-Z>         - to exit from facility'
      WRITE (*, *)
     &          'one or more <CR> - pop up one or more levels until you'
      WRITE (*, *)
     &            '                   exit from facility (at the lowest'
      WRITE (*, *) '                   level the first <CR> gives the '
      WRITE (*, *)
     &           '                   (sub-)topic list at current level)'
      WRITE (*, *) '?   - to see (sub-)topic list at current level'
      WRITE (*, *)
     &          '/   - skip to next (sub-)topic (you can go through the'
      WRITE (*, *) '      complete help file by repeating /)'
      WRITE (*, *) '>   - skip to following topic at current level'
      WRITE (*, *) '<   - go to prev. topic at current level'
      WRITE (*, *) '^   - pop up one level'
      WRITE (*, *) 'or any of the sub-topic names'
      RETURN
      END
