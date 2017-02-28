
      SUBROUTINE clcpps(wmoff, wbinfac, wmstrt, wmend, optaxs, 
     &                  abnsze, srcdet, source, simple, hist)

      INTEGER wmoff(2), wbinfac, wmstrt(2), wmend(2)
      REAL optaxs(2)
      REAL srcdet(2), source(2), abnsze
      character(80) hist(3)
      LOGICAL simple

c Subroutine to calculate the source position in polar coordinates
c relative to the optical axis. If the simple flag is set then take
c the position as the center of the wmap otherwise use that in SRCDET.

      DOUBLE PRECISION pi
      PARAMETER (pi=3.14159265358979323846264338327950d0)

      REAL xdist, ydist

      IF ( simple ) THEN

         srcdet(1) = wmoff(1)-1 + ((wmend(1)+wmstrt(1))/2.)*wbinfac
         srcdet(2) = wmoff(2)-1 + ((wmend(2)+wmstrt(2))/2.)*wbinfac

      ENDIF

      xdist = (optaxs(1) - srcdet(1))/wbinfac
      ydist = (optaxs(2) - srcdet(2))/wbinfac

      source(1) = abnsze*sqrt(xdist**2+ydist**2)
         
      IF ( xdist.NE.0 .AND. ydist.NE.0 ) THEN
         source(2) = ATAN2(ydist, xdist) * 180./pi
      ELSE
         source(2) = 0.
      ENDIF
      IF (source(2) .LT. 0) source(2) = source(2) + 360.


      WRITE(hist(1),'(a,f7.2,1x,f7.2,1x,a)') ' Point source at ', 
     &     srcdet(1), srcdet(2), '(detector coordinates)'
      WRITE(hist(2),'(a,f7.2,1x,f7.2,1x,a)') ' Point source at ', 
     &     xdist, ydist, '(WMAP bins wrt optical axis)'
      WRITE(hist(3),'(a,f7.2,1x,f7.2,1x,a)') ' Point source at ', 
     &     source(1), source(2), 
     &     '(... in polar coordinates)'

      RETURN
      END





