**==PREC.FOR
      SUBROUTINE prec(ra,dec,jy,l)
       
c
c       subroutine to precess celestial coordinates by providing an
c       interface to sla_preces
c
c       ra and dec in degrees
c       jy = year (e.g. 2000)
c       FK5 precession from jy to 1950 if l=1
c       FK5 precession from 1950 to jy if l=2
c       FK4 precession from jy to 1950 if l=3
c       FK4 precession from 1950 to jy if l=4
c
      INTEGER*4 jy, l
      REAL*8 ra, dec, epoch1, epoch2, rar, decr
      character(3) system
c
      IF ( l.EQ.1 .or. l.eq.3 ) THEN
        epoch1 = jy
        epoch2 = 1950.D0
      ELSE IF ( l.EQ.2 .or. l.eq.4 ) THEN
        epoch1 = 1950.D0
        epoch2 = jy
      ELSE
        write(*,1)l
1       format(' Error in prec, invalid switch = ',i6)
        return
      END IF
c
      if( l.le.2 ) then
       system = 'FK5'
      else
       system = 'FK4'
      endif
c
c convert to radians
c
      call degrad(ra,dec,rar,decr)
      CALL sla_preces (system, epoch1, epoch2, rar, decr)
      call raddeg(rar,decr,ra,dec)
c
      RETURN
      END
