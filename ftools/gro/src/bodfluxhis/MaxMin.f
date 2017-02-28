C      
C      subroutine MaxMin - find max value
C
C******************************************************************************
      subroutine MaxMin(x,npt, max, min)
      
      integer i
      real    x(npt), max,min

      max = x(1)
      min = x(1)
      do i = 2, npt
         if (x(i) .gt. max ) then 
            max = x(i)
         endif
         if (x(i) .lt. min ) then 
            min = x(i)
         endif
      enddo
      return 
      end
