C******************************************************************************
C SUBROUTINE:
C      rd2sat
C
C DESCRIPTION:
C      transform from celestial ra, dec to satellite based theta, phi, roll 
C
C AUTHOR/DATE:
C       Emily A. Greene    December 1992
C	Hughes STX
C
C MODIFICATION HISTORY:
C
C       Re-written to allow for mis-alignment of telescope/boresight
C       via mis-alignment matrix. E. Gotthelf (GSFC) and Fujimoto-san 
C       (ISAS), April 1993.
C
C       Roll angle computation by Fujimoto-san (ISAS) added Oct 7 1993.
C
C       Modified to allow for machine roundoff errors, E. Gotthelf (GSFC)
C       Oct 30 1993. 
C
C NOTES:
C	The transformation is (12-13a) in "Spacecraft
C	Attitude Determination and Control" ed. J. R. Wertz. 
C
C USAGE:
C      call rd2sat (ra, dec, q, theta, phi, roll, error)
C
C ARGUMENTS:
C   input:
C	q     - quaternions of the satellite attitude
C	ra    - celestrial right ascension in radians
C	dec   - celestrial declanation in radians
C   output:
C	theta - Angle from telescope axis in radians
C	phi   - Angle around telescope axis in radians
C       roll  - Spacecraft roll angle in radians
C       error - Error flag
C
C PRIMARY LOCAL VARIABLES:
C	a,b,c - RA and DEC transformed to cartesian vectors
C	axx   - transformation matrix elements
C
C CALLED ROUTINES:
C
C******************************************************************************

	subroutine rd2sat (ra, dec, q, theta, phi, roll, error)

        implicit none

	real ra, dec
	double precision q(4)
	real theta, phi, roll
        logical error

        include 'asca_defs.inc'

        double precision cos_dec, boresight(3,3), uvw(3),sky(3),xrt(3,3)
        double precision r, atheta, aphi1, aphi2, euler1, euler2, euler3

        include 'asca_common.inc'

C quaternion transformation matrix elements

        call dir_cos(q, boresight)
        
C RA and DEC transformed to cartesian vectors
C       DEC: +-90 degrees; RA: 0-360 degrees
        
        cos_dec = cos(dble(dec))
	sky(1) = cos(dble(ra)) * cos_dec
	sky(2) = sin(dble(ra)) * cos_dec
	sky(3) = sin(dble(dec))

C compensate for boresight / telescope optical axis mis-alignment

        call matrix_mult3(misalign, boresight, xrt)

C convert to Euler angle

        call cosine2euler(xrt, euler1, euler2, euler3, error)

C project sky coords into cartisian coords of telescope frame

        call matrix_proj3(xrt, sky, uvw)

C transfrom to theta, phi coordinates 
C	theta define to be 0 along +z, 180 along -z
C	phi defined to be 0 along +x

        r = sqrt(uvw(1)**2+uvw(2)**2+uvw(3)**2)

        aphi1 = uvw(1)/r
        aphi2 = uvw(2)/r
        atheta = uvw(3)/r

        if (abs(atheta) + epsilon .lt. 1.0D0) then
           theta = real(acos(atheta))
           if (abs(aphi2).gt.epsilon.or.abs(aphi1).gt.epsilon) then
              phi = real(atan2(aphi2, aphi1))
           else 
              phi = 0.0
              error = .TRUE.
           end if
        else 
           phi = 0.0
           if (atheta .lt. -0.5D0) then
              theta = real(pi)
           else
              theta = 0.0
           end if
        end if
 
        roll = -real(halfpi - euler3)
        if (roll .lt. -pi) roll = roll + twopi

	return
	end
        


