*- xdelta - delta string from declination in degrees
      character(80) function xdelta(dec,status)
       
      include 'status.codes'

* Import :
      real*8 dec
* Status :
      integer status
* Local variables :
      real*8 d
      integer degree,arcmin
      real*8 arcsec
      character(80) a
*-
      if(status.ne.ok__)return

      if(dec.le.0d0)then
         a='-'
         d=-dec
      else
         a=' '
         d=dec
      endif
      degree=int(d)
      arcmin=int(6d1*(d-dble(degree)))
      arcsec=6d1*6d1*(d-dble(degree)-dble(arcmin)/6d1)

      write(a(2:3),'(i2.2)',iostat=status)degree
      write(a(5:6),'(i2.2)',iostat=status)arcmin
      write(a(8:12),'(f5.2)',iostat=status)arcsec
      if(a(8:8).eq.' ')a(8:8)='0'

      xdelta=a

      return

      end
