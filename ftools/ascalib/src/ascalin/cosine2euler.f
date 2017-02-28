C******************************************************************************
C FUNCTION:
C     cosine2euler
C
C DESCRIPTION: 
C
C     converts a cosine transformation matrix to a quaterian
C     from atFunction by ay, modified by n. kawai
C
C     corrected psi calculation. Dec 22 1993, E. Gotthelf.
C     revised range of phi, psi to +/-pi psi. Jun 25 1993, E. Gotthelf.
C
C ARGUMENTS:
C     z - cosine transformation matrix (input)
C     q - quaterian (output)
C
C******************************************************************************

      subroutine cosine2euler(z, phi, theta, psi, error)
      double precision  z(3,3), phi, theta, psi
      logical error

       double precision pi, SMALL

       parameter (pi = 3.141592654)
       parameter (SMALL = 1.0E-12)

       if (z(3,3) .gt. 1.0d0 - SMALL) then
          
          theta = 0.0d0
          phi = atan2(z(1,2), z(1,1))
          psi = 0.0d0
          
       else if (z(3,3) .lt. -1.0d0 + SMALL) then

          theta = pi
          phi = atan2(-z(1,2), -z(1,1))
          psi = 0.0d0

       else
             
          theta = acos(z(3,3))

          if (abs(z(3,1)) .gt. SMALL .or. abs(z(3,2)) .gt. SMALL) then
             phi = atan2(z(3,2), z(3,1))
          else
             phi = 0.0d0
             error = .TRUE.
          end if

          if (abs(z(1,3)) .gt. SMALL .or. abs(z(2,3)) .gt. SMALL) then
             psi = atan2(z(2,3), -z(1,3))
          else
             psi = 0.0d0
             error = .TRUE.
          end if
          
       end if

c       if (phi .lt. 0.0) phi = phi + 2.0d0 * pi
c       if (psi .lt. 0.0) psi = psi + 2.0d0 * pi

       end


