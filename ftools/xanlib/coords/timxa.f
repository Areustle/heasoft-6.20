*- timxa.for - convert string "dd-mmm-yyyy hh:mm:ss" to i*2 array(5)
      subroutine timxa(string,it)
* Author :
*  Andy Pollock
* History :
*  6 October 1992 : original
*  17 Jun  1998 : modified to write the four digits year. Ning Gan

* Import :
      character*(*) string
* Export :
      integer*4 it(*)
* Local variables :
      character(80) s
      character(3) month
      integer*4 status
      integer*4 m
      integer*4 dom
      integer*4 n
* External reference :
      character(3) xmonth
*-
      s=string
      it(1)=0
      read(s(8:11),'(i4)',iostat=status)it(1)
C      it(1)=it(1)+1900

      call upc(s(4:6))
      month=' '
      n=0
      do while((month.ne.s(4:6)).and.(n.lt.12))
         n=n+1
         month=xmonth(n)
         call upc(month)
      end do
      m=n
      read(s(1:2),'(i2)',iostat=status)dom
      call xmdyd(m,dom,it(1),it(2))

      it(3)=0
      read(s(13:14),'(i2)',iostat=status)it(3)
      it(4)=0
      read(s(16:17),'(i2)',iostat=status)it(4)
      it(5)=0
      read(s(19:20),'(i2)',iostat=status)it(5)

      return

      end
