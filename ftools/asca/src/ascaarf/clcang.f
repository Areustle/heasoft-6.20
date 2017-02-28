
      SUBROUTINE clcang(wmsiz1, wmsiz2, wmoff, wbinfac, optaxs, 
     &                  abnsze, theta, phi)

      INTEGER wmsiz1, wmsiz2, wmoff(2), wbinfac
      REAL optaxs(2)
      REAL theta(wmsiz1, wmsiz2), phi(wmsiz1, wmsiz2)
      REAL abnsze

c Subroutine to calculate positions in polar coordinates relative to the 
c optical axis.

c Arguments :
c     wmsiz1    i       i: Actual size of WMAP
c     wmsiz2    i       i: Actual size of WMAP
c     wmoff     i       i: WMAP offset in detector coordinates
c     wbinfac   i       i: WMAP bin factor
c     optaxs    i       i: Optical axis in detector coordinates
c     abnsze    r       i: The size (arcmin) of each WMAP bin
c     theta     r       r: Angle in arcmin from optical axis
c     phi       r       r: Azimuthal angle in degrees.

      DOUBLE PRECISION pi
      PARAMETER (pi=3.14159265358979323846264338327950d0)

      REAL xdist, ydist

      INTEGER j, k

*find the (theta,phi) position of each pixel in wmap (which goes from
*(1,1) to (wmsiz1,wmsiz2)) w.r.t. the middle. Assume theta is small
*enough that can treat the arcs as line segments. wmoff is the first 
*detector image pixel in the wmap.
*theta and phi are defined like this: looking at the detector face on,
*centered, and 3500.0 mm away. Theta is the angle subtended by the
*distance between a point on the detector and the center. Phi is the angle 
*made by the x-axis extended from the center and the segment between the point
*and the center (phi is an element of [0,360)).

      DO k = 1, wmsiz2

         ydist = optaxs(2) - wmoff(2)
     &               - ( (k-1)*wbinfac + 0.5*(wbinfac-1) )
         ydist = ydist/wbinfac

         DO j = 1, wmsiz1

            xdist = optaxs(1) - wmoff(1) 
     &               - ( (j-1)*wbinfac + 0.5*(wbinfac-1) )
            xdist = xdist/wbinfac


            theta(j,k) = abnsze*sqrt(xdist**2+ydist**2)

            IF ( xdist.NE.0 .OR. ydist.NE.0 ) THEN
               phi(j,k) = ATAN2(ydist, xdist) * 180./pi
            ELSE
               phi(j,k)=0
            ENDIF
            IF (phi(j,k) .LT. 0) phi(j,k) = phi(j,k) + 360.

         ENDDO

      ENDDO

      RETURN
      END
