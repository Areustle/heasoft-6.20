C      
C      subroutine getMaxMin - find max & min value in 2D data
C
C******************************************************************************
      subroutine getMaxMin(x,npt1,npt2, max, min)
      
      integer i,j, npt1, npt2
      double precision x(npt1,npt2)
      real   max,min
      
      max = x(1,1)
      min = x(1,1)
      do j=1,npt2
         do i = 1, npt1
            if (x((i-1)*8+j,1) .gt. max ) then 
               max = x((i-1)*8+j,1)
            endif
            if (x((i-1)*8+j,1) .lt. min ) then 
               min = x((i-1)*8+j,1)
            endif
         enddo
      enddo

      return 
      end
