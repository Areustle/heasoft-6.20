*- xkddat - 'yyyy.ddd hh:mm:ss' from shf key
      character*(*) function xkddat(shf)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 shf
* Local variables :
      integer*4 t(5)
      character(17) s
      integer*4 status
*-
      call timka(shf,t)
      s='yyyy.ddd hh:mm:ss'
      write(s(1:4),'(i4.4)',iostat=status)t(1)
      write(s(6:8),'(i3.3)',iostat=status)t(2)
      write(s(10:11),'(i2.2)',iostat=status)t(3)
      write(s(13:14),'(i2.2)',iostat=status)t(4)
      write(s(16:17),'(i2.2)',iostat=status)t(5)

      xkddat=s

      return

      end
