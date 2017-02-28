C******************************************************************************
C SUBROUTINE:
C
C     sun_sat_ang
C     
C     ASCA X-Y-Z satellite pointing and Solar angles.
C     All angles in radians. ascatime in ascatime.
C
C     By E. Gotthelf, Apr 16, 1994.
C     streamlined June 1994, EVG.
C     ecliptic bug corrected Aug 1995, EVG.
C
C******************************************************************************
 
        subroutine sun_sat_ang (ascatime, euler, x_axis, y_axis, z_axis, 
     &     sun, aber)
        implicit none

	double precision ascatime
        real euler(3), x_axis(3), y_axis(3), z_axis(3), sun(3), aber(3)
        
        integer i, j

        real solar_long, sep_ang_rad

        double precision phi, theta, psi
        double precision boresight(3,3), z(3,3), uvw(3)
        double precision x_unit(3), y_unit(3), z_unit(3), xyz(3)
        double precision k, jd, jd_zero, ara1, ara2, adec
        double precision cos_ra, sin_ra, cos_dec, sin_dec, ecliptic
        double precision cos_lon, sin_lon, cos_ecliptic, sin_ecliptic
	double precision pi, halfpi, twopi, epsilon, deg_to_rad

        parameter( pi = 3.1415926535898D0)
        parameter( halfpi = 1.5707963267949D0)
        parameter( twopi = 6.2831853071796D0)
        parameter( epsilon = 1.0E-12)
        parameter( deg_to_rad = 0.0174532925199D0)

        parameter (k = 20.496D0 / 3600.0D0 * deg_to_rad)
        parameter (jd_zero = 2448987.5d0)

        data x_unit / 1.0D0, 0.0D0, 0.0D0 /
        data y_unit / 0.0D0, 1.0D0, 0.0D0 /
        data z_unit / 0.0D0, 0.0D0, 1.0D0 /

        jd = ascatime/86400.0d0 + jd_zero

c     J2000
        ecliptic = 23.439281d0 * deg_to_rad

        phi   = dble(euler(1))
        theta = dble(euler(2))
        psi   = dble(euler(3))

        call solar(jd, sun)

        cos_ra  = cos(phi)
        sin_ra  = sin(phi)        
        cos_dec = cos(halfpi-theta)
        sin_dec = sin(halfpi-theta)
        cos_lon = cos(sun(3))
        sin_lon = sin(sun(3))
        cos_ecliptic = cos(ecliptic)
        sin_ecliptic = sin(ecliptic)

        if (abs(cos_dec) .gt. epsilon) then 
           aber(1) = (-k / cos_dec) * 
     &          (sin_lon*sin_ra + cos_lon*cos_ecliptic*cos_ra)
        else 
           aber(1) = 0.0
        end if
        aber(2) = k * ( -sin_lon * cos_ra * sin_dec + cos_lon *
     &       (cos_ecliptic*sin_ra*sin_dec-sin_ecliptic*cos_dec))
        aber(3) = sun(3)

        z(1,1) =  cos(psi)*cos(theta)*cos(phi) - sin(psi)*sin(phi) 
        z(2,1) =  cos(psi)*cos(theta)*sin(phi) + sin(psi)*cos(phi) 
        z(3,1) = -cos(psi)*sin(theta)      
        z(1,2) = -sin(psi)*cos(theta)*cos(phi) - cos(psi)*sin(phi)
        z(2,2) = -sin(psi)*cos(theta)*sin(phi) + cos(psi)*cos(phi)
        z(3,2) =  sin(psi)*sin(theta)
        z(1,3) = sin(theta)*cos(phi)
        z(2,3) = sin(theta)*sin(phi)
        z(3,3) = cos(theta)

        call matrix_proj3(z, x_unit, uvw)
        if (uvw(3) + epsilon .gt. 1.0D0) then
           x_axis(2) = real(halfpi)
        else if (uvw(3) - epsilon .lt. -1.0D0) then
           x_axis(2) = -real(halfpi)
        else
           x_axis(2) = real(asin(uvw(3)))
        end if
        if (abs(uvw(1)).gt.epsilon.or.abs(uvw(2)).gt.epsilon) then
           x_axis(1) = real(atan2(uvw(2), uvw(1)))
           if (z_axis(1) .lt. 0.0) x_axis(1) = x_axis(1) + twopi
        else 
           x_axis(1) = 0.0
        end if
        call matrix_proj3(z, y_unit, uvw)
        if (uvw(3) + epsilon .gt. 1.0D0) then
           y_axis(2) = real(halfpi)
        else if (uvw(3) - epsilon .lt. -1.0D0) then
           y_axis(2) = -real(halfpi)
        else
           y_axis(2) = real(asin(uvw(3)))
        end if
        if (abs(uvw(1)).gt.epsilon.or.abs(uvw(2)).gt.epsilon) then
           y_axis(1) = real(atan2(uvw(2), uvw(1)))
           if (y_axis(1) .lt. 0.0) y_axis(1) = y_axis(1) + twopi
        else 
           y_axis(1) = 0.0
        end if
        call matrix_proj3(z, z_unit, uvw)
        if (uvw(3) + epsilon .gt. 1.0D0) then
           z_axis(2) = real(halfpi)
        else if (uvw(3) - epsilon .lt. -1.0D0) then
           z_axis(2) = -real(halfpi)
        else
           z_axis(2) = real(asin(uvw(3)))
        end if
        if (abs(uvw(1)).gt.epsilon.or.abs(uvw(2)).gt.epsilon) then
           z_axis(1) = real(atan2(uvw(2), uvw(1)))
           if (z_axis(1) .lt. 0.0) z_axis(1) = z_axis(1) + twopi
        else 
           z_axis(1) = 0.0
        end if
        x_axis(3) = sep_ang_rad(x_axis(1), x_axis(2), sun(1), sun(2))
        y_axis(3) = sep_ang_rad(y_axis(1), y_axis(2), sun(1), sun(2))
        z_axis(3) = sep_ang_rad(z_axis(1), z_axis(2), sun(1), sun(2))

	end

******************************************************************************
C SUBROUTINE:
C
C     solar
C        
C DESCRIPTION:
C
C     apparent solar equitorial and ecliptic coords for given JD time.
C
C AUTHOR/DATE:
C     
C     Eric Gotthelf, April 1993.
C     ASCA GOF, NASA/GSFC,
C     
C MODIFICATION HISTORY:
C     
C USAGE:
C
C     call solar(jd, sun)
C
C ARGUMENTS:
C
C     jd                   - Julian date
C     sun(3)               - Solar R.A., DEC., longitude (radians)
C
C REFERENCE:
C
C     Nautical almanac 1993, sections B and C.
C
C NOTES:
C
C     Low precision.
C
C******************************************************************************

      subroutine solar(jd, sun)
        
      implicit none
      
      double precision jd
      real sun(3)
      
      double precision k, g, L, ecliptic, sunl, sunb, cose, sine
      double precision lb2ad(3,3), sunlb(3), uvw(3), T
      double precision pi, halfpi, twopi, epsilon, deg_to_rad
      
      parameter( pi = 3.1415926535898D0)
      parameter( halfpi = 1.5707963267949D0)
      parameter( twopi = 6.2831853071796D0)
      parameter( epsilon = 1.0E-12)
      parameter( deg_to_rad = 0.0174532925199D0)
      
      data lb2ad /1.0d0,  0.0d0     , 0.0d0, 
     &            0.0d0,  0.917434d0, 0.397888d0,
     &            0.0d0, -0.397888d0, 0.917434d0/
      
      T = jd - 2451545.0D0
 
      g = 6.28318D0 + mod(357.528D0 + real(0.9856003d0 * T), 360.000D0) 
     &     * deg_to_rad

      L = 360.000D0 + mod(280.460D0 + real(0.9856474D0 * T), 360.000D0)
      
      ecliptic = (23.439 - 0.0000004d0 * T) * deg_to_rad

      sunl =  mod(L+1.915D0*sin(g)+0.020D0*sin(2.0D0*g), 360.000D0) 
     &     * deg_to_rad
      
      sine = sin(ecliptic)
      cose = cos(ecliptic)

      lb2ad(2,2) =  cose
      lb2ad(3,3) =  cose
      lb2ad(2,3) = -sine
      lb2ad(3,2) =  sine

      sunb = 0.0D0
      sun(3) = sunl
      sunlb(1) = cos(sunl) 
      sunlb(2) = sin(sunl)
      sunlb(3) = 0.0d0
      
      call matrix_proj3(lb2ad, sunlb, uvw)

      if (uvw(3) + epsilon .gt. 1.0D0) then
         sun(2) = real(halfpi)
      else if (uvw(3) - epsilon .lt. -1.0D0) then
         sun(2) = -real(halfpi)
      else
         sun(2) = real(asin(uvw(3)))
      end if
      if (abs(uvw(1)).gt.epsilon.or.abs(uvw(2)).gt.epsilon) then
         sun(1) = real(atan2(uvw(2), uvw(1)))
         if (sun(1) .lt. 0.0) sun(1) = sun(1) + twopi
      else 
         sun(1) = 0.0
      end if
      
      end

C*****************************************************************************
c     SUBROUTINE SEP_ANG_RAD
c
c     by Eric Gotthelf at NASA/GSFC -- Apr. 1994
c
c     This subroutine gives the seperation between two 
c     sky points. Single precision. All angles in radians
C*****************************************************************************

      real function sep_ang_rad(ra1, dec1, ra2, dec2) 

      implicit none

      real ra1, dec1, ra2, dec2
      
c     decimal radians

      double precision small, pi
      double precision cos_d, delta_ra, delta_dec, d
      parameter (small = 0.003D0)
      parameter( pi = 3.1415926535898D0)
 
      delta_ra = dble(ra1 - ra2)
      delta_dec = dble(dec1 - dec2)
      
      d = sqrt((cos(dble(dec1))*delta_ra)**2+delta_dec**2)
      
      if (.not. (d .lt. small .or. abs(d-pi) .lt. small)) then
         cos_d = sin(dble(dec1)) * sin(dble(dec2)) +
     &        cos(dble(dec1)) * cos(dble(dec2)) * 
     &        cos(delta_ra)
         d = acos( max( min( cos_d, 1.00000000000000D0), 
     &        -1.0000000000000D0))
      end if
            
      sep_ang_rad = (d)

      end


