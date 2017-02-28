*- xalpha - alpha string from ra in degrees
      character(80) function xalpha(ra,status)
       
      include 'status.codes'

* Import :
      real*8 ra
* Status :
      integer status
* Local variables :
      integer hour,minute
      real*8 second
      character(80) a
*-
      if(status.ne.ok__)return

      hour=int(ra/15d0)
      minute=int(4d0*(ra-15d0*dble(hour)))
      second=6d1*4d0*(ra-15d0*(dble(hour)+dble(minute)/6d1))

      a=' '
      write(a(1:2),'(i2.2)',iostat=status)hour
      write(a(4:5),'(i2.2)',iostat=status)minute
      write(a(7:12),'(f6.3)',iostat=status)second
      if(a(7:7).eq.' ')a(7:7)='0'

      xalpha=a

      return

      end
