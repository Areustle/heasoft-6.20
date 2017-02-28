*- timsa.for - convert string yyyy ddd hh:mm:ss to i*2 array(5)
      subroutine timsa(string,it)
* Author :
*  Andy Pollock
* History :
*  6 October 1992 : original

* Import :
      character*(*) string
* Export :
      integer*4 it(*)
* Local variables :
      character(80) s
      integer*4 status
*-
      s=string
      it(1)=0
      read(s(1:4),'(i4)',iostat=status)it(1)
      it(2)=0
      read(s(6:8),'(i3)',iostat=status)it(2)
      it(3)=0
      read(s(10:11),'(i2)',iostat=status)it(3)
      it(4)=0
      read(s(13:14),'(i2)',iostat=status)it(4)
      it(5)=0
      read(s(16:17),'(i2)',iostat=status)it(5)

      return

      end
