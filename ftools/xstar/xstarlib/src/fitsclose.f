      subroutine fitsclose(lun11,unit,status)
C
C     Close the file & release the unit number
c     author: T. Bridgman
C
C     Parameters:
C        unit    integer            File unit number
C        status  integer            Returned status code
c
      implicit none
      integer unit, status,lun11
c
c      write (6,*)'closing fits unit ',unit
      call ftclos(unit, status)
      call ftfiou(unit, status)
      call frelunx(unit)
      close(unit)
      if (status .gt. 0)call printerror(lun11,status)
c
      return
      end
