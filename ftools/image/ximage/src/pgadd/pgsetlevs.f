C   Subroutine PGSETLEVS
C
C   12/16/97 - MSJ - Copies arguments into levels common
C
   
      subroutine pgsetlevs(no_of_levels,lev_ary)
      implicit none
      integer no_of_levels
      real lev_ary(*)
      integer i

      include 'level.inc'

      do i = 1,no_of_levels
          LEVels(i) = lev_ary(i)
      enddo
      NUMlevs = no_of_levels

      return
      end
       
