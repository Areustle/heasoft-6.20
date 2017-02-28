*- xkdate - 'yyyy-MMM-dd hh:mm:ss' from shf key
      character*(*) function xkdate(shf)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 shf
* Local variables :
      integer*4 t(5)
      integer*4 month,day
      character(20) s
      integer*4 status
* External reference :
      character(20) xmonth
*-
      call timka(shf,t)
      s='yyyy-MMM-dd hh:mm:ss'
      write(s(1:4),'(i4.4)',iostat=status)t(1)
      write(s(6:8),'(i3.3)',iostat=status)t(2)
      call xydmd(t(1),t(2),month,day)
      s(6:8)=xmonth(month)
      write(s(10:11),'(i2.2)',iostat=status)day
      write(s(13:14),'(i2.2)',iostat=status)t(3)
      write(s(16:17),'(i2.2)',iostat=status)t(4)
      write(s(19:20),'(i2.2)',iostat=status)t(5)

      xkdate=s

      return

      end
