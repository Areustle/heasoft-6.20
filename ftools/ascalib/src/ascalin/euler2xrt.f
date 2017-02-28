C******************************************************************************
C SUBROUTINE:
C
C     EULER2XRT
C     
C DESCRIPTION:
C     
C     Subroutine to convert from ASCA Euler angle to R.A., Dec, and Roll 
C     angle corrected for mean boresight misalignment. Angles in degrees,
C     0<ra<360, -90<dec<90, 0<roll<360.
C     
C AUTHOR/DATE:
C     
C     Eric Gotthelf, ASCA GOF, NASA/GSFC.
C     
C     May 18 1993.
C     
C******************************************************************************
        
      subroutine EULER2XRT(m, phi_r, theta_r, psi_r, ra, dec, roll, 
     &       error)
        
      implicit none
        
      double precision m(3,3)
      real phi_r, theta_r, psi_r, ra, dec, roll
      logical error
      integer i, j, k 
      double precision r, d, phi, theta, psi, qi, sum
      double precision q(4), z(3,3), w(3,3), v(3), y(3)
      double precision pi, halfpi, twopi, epsilon, deg_to_rad
      parameter( pi = 3.1415926535898D0)
      parameter( halfpi = 1.5707963267949D0)
      parameter( twopi = 6.2831853071796D0)
      parameter( epsilon = 1.0D0-12D0)
      parameter( deg_to_rad = 0.0174532925199D0)
      
C     initialize:

      error = .FALSE.
      phi = phi_r * deg_to_rad
      theta = theta_r * deg_to_rad
      psi = psi_r * deg_to_rad
      r = phi
      d = (halfpi - theta)

c     cartisian coords:

      y(1) = -cos(r) * sin(d)
      y(2) = -sin(r) * sin(d)
      y(3) = cos(d)

c     convert to quaterians:

      z(1,1) =  cos(psi)*cos(theta)*cos(phi) - sin(psi)*sin(phi)
      z(1,2) =  cos(psi)*cos(theta)*sin(phi) + sin(psi)*cos(phi)
      z(1,3) =  -cos(psi)*sin(theta)      
      z(2,1) = -sin(psi)*cos(theta)*cos(phi) - cos(psi)*sin(phi)
      z(2,2) = -sin(psi)*cos(theta)*sin(phi) + cos(psi)*cos(phi)
      z(2,3) =  sin(psi)*sin(theta)
      z(3,1) =  sin(theta)*cos(phi)
      z(3,2) =  sin(theta)*sin(phi)
      z(3,3) =  cos(theta)

      qi = 1.0D0 + z(1,1) + z(2,2) + z(3,3)
      if (qi .gt. epsilon) then         
         q(4) = 0.5d0 * sqrt(qi) 
         q(1) = (z(2,3) - z(3,2)) / (4.0d0 * q(4)) 
         q(2) = (z(3,1) - z(1,3)) / (4.0d0 * q(4)) 
         q(3) = (z(1,2) - z(2,1)) / (4.0d0 * q(4)) 
      else
         q(4) = 0.0d0
         qi = 1.0d0 + z(1,1) - z(2,2) - z(3,3)
         if (qi .gt. epsilon) then
            q(1) = 0.5d0 * sqrt(qi) 
            q(2) = (z(1,2) + z(2,1)) / (4.0d0 * q(1)) 
            q(3) = (z(1,3) + z(3,1)) / (4.0d0 * q(1)) 
         else
            q(1) = 0.0d0
            qi = 1.0d0 - z(1,1) + z(2,2) - z(3,3)
            if (qi .gt. epsilon) then
               q(2) = 0.5d0 * sqrt(qi) 
               q(3) = (z(2,3) + z(3,2)) / (4.0d0 * q(2)) 
            else
               q(2) = 0.0d0
               q(3) = 1.0d0                
            end if
         end if
      end if
      
c     compensate for mis-alignment:

      call dir_cos(q, z)
      call matrix_mult3(m, z, w)

c     ra, dec, roll:

      call cosine2euler(w, phi, theta, psi, error)

      if (phi .lt. 0.0) then
         ra = real((phi + twopi) / deg_to_rad)
      else
         ra = real(phi / deg_to_rad)
      end if
      
      if (psi .lt. 0.0) then
         roll = -real((halfpi - psi + twopi)/ deg_to_rad) 
      else
         roll = -real((halfpi - psi) / deg_to_rad)
      end if
      if (roll .lt. 0.0) roll = roll + 360.0
      
      dec  =  real((halfpi - theta) / deg_to_rad)
      
      end



