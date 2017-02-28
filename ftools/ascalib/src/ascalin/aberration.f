C******************************************************************************
C FUNCTION:
C
C     aberration
C        
C DESCRIPTION:
C
C     Annual aberration correction for ASCA time.
C
C AUTHOR/DATE:
C     
C     Eric Gotthelf, Nov 1993.
C     ASCA GOF, NASA/GSFC,
C     
C MODIFICATION HISTORY:
C     
C USAGE:
C
C     call aberration(ascatime, ra, dec, delta_ra, 
C     &     delta_dec, sun_long)
C
C ARGUMENTS:
C
C     ascatime             - ASCA time, E1993.0D0 (seconds)
C     ra, dec              - Geometric geocentric ra/dec (radians).
C     delta_ra, delta_dec  - Correction for annular aberration to the 
C                            geometric geocentric ra/dec in the sense,
C                            delta = apparent - true, (radians).
C     sun_long             - Solar longitude (radians)
C
C REFERENCE:
C
C     Nautical almanac 1993, sections B and C.
C
C******************************************************************************

      subroutine aberration(ascatime, ra, dec, delta_ra, 
     &     delta_dec, sun_long)
      
      implicit none
      
      double precision ascatime
      real ra, dec, delta_ra, delta_dec, sun_long
      
      real k, g, L, deg_to_rad, epsilon
      real cos_ra, cos_dec, cos_ecliptic, cos_lon
      real sin_ra, sin_dec, sin_ecliptic, sin_lon
      double precision T, jd_1900, jd_zero
      
      parameter (epsilon = 1.0E-12)
      parameter (jd_zero = -2557.5d0)
      parameter (cos_ecliptic = 0.9174076)
      parameter (sin_ecliptic = 0.3979486)
      parameter (deg_to_rad = 0.017453229)
      parameter (k = 20.496 / 3600.0 * deg_to_rad)
      
      cos_ra = cos(ra)
      sin_ra = sin(ra)
      
      cos_dec = cos(dec)
      sin_dec = sin(dec)
      
      T = ascatime/86400.0d0 + jd_zero

      g = 6.28318 + mod(357.528 + real(0.9856003d0 * T), 360.000) 
     &     * deg_to_rad

      L = 360.000 + mod(280.460 + real(0.9856474D0 * T), 360.000)
      
      sun_long =  mod(L+1.915*sin(g)+0.020*sin(2.0*g), 360.000) 
     &     * deg_to_rad
      
      cos_lon = cos(sun_long)
      sin_lon = sin(sun_long)
      
      if (abs(cos_dec) .gt. epsilon) then 
         
         delta_ra = (-k / cos_dec) * 
     &        (sin_lon*sin_ra + cos_lon*cos_ecliptic*cos_ra)
         
      else 
         
         delta_ra = 0.0
         
      end if
      
      delta_dec = k * ( -sin_lon * cos_ra * sin_dec + cos_lon *
     &     (cos_ecliptic * sin_ra * sin_dec - sin_ecliptic * cos_dec) )
      
      end
