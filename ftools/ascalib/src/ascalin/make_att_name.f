      subroutine make_att_name(aspname, pathname, frfname)

      implicit none

      character*(*) aspname, pathname, frfname

      integer i, j, k

      frfname(2:2) = 'a'     
      
      i = min(len(pathname), len(aspname))
      do j=1,i
         aspname(j:j) = pathname(j:j)
      end do                       

      do while (i.gt. 0 .and. pathname(i:i) .eq. ' ') 
         i = i - 1
      end do
      j = i + 1

      if (j .gt. 1) then 
c         aspname(j:j) = '/'
         k = min(len(aspname)-j-1, 18)
         k = min(len(frfname), k)
         do i = 1, k
            aspname(i+j-1:i+j-1) = frfname(i:i)
         end do
      else
         k = min(len(frfname), len(aspname))
         do i = 1, k
            aspname(i:i) = frfname(i:i)
         end do
      end if
      
      end
      
      
