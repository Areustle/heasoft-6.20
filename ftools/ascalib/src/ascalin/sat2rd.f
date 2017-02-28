C******************************************************************************
C SUBROUTINE:
C      sat2rd
C
C DESCRIPTION:
C      transform from satellite based theta,phi to celestial ra, dec
C
C AUTHOR/DATE:
C       Emily A. Greene    April 1993
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
C	The transformation is the inverse (or transpose) of (12-13a) in 
C	"Spacecraft Atttitude Determination and Control" ed. J. R. Wertz. 
C
C USAGE:
C      call sat2rd (ra, dec, q, theta, phi, error)
C
C ARGUMENTS:
C   input:
C	theta - Angle from telescope axis in radians
C	phi   - Angle around telescope axis in radians
C	q     - quaternions of the satellite attitude
C   output:
C	ra    - celestial right ascension in radians
C	dec   - celestiral declanation in radians
C       error - error flag
C
C PRIMARY LOCAL VARIABLES:
C	a,b,c - RA and DEC transformed to cartesian vectors
C	axx   - transformation matrix elements
C
C CALLED ROUTINES:
C
C******************************************************************************

	subroutine sat2rd (ra, dec, q, theta, phi, error)
        
        implicit none

	real ra, dec
	double precision q(4)
	real theta, phi
        logical error

        include 'asca_defs.inc'

        double precision iboresight(3,3), xrt(3,3), det(3), xyz(3)
	double precision r, adec, ara1, ara2

        include 'asca_common.inc'

C Theta and Phi spacecraft transformed to cartesian vectors
C	theta define to be 0 along +z, 180 along -z
C	phi defined to be 0 along +x
C values should be in radians 

	det(1) = dble(cos(dble(phi))*sin(dble(theta)))
	det(2) = dble(sin(dble(phi))*sin(dble(theta)))
	det(3) = dble(cos(dble(theta)))

C quaternion transformation matrix elements

        call dir_cos_inv(q, iboresight)

C compensate for boresight / telescope optical axis mis-alignment

        call matrix_mult3(iboresight, imisalign, xrt)

C project detector coords into cartisian coords of telescope frame 

        call matrix_proj3(xrt, det, xyz)

C transfrom to RA, DEC coordinates
C	DEC is +-90 radians
C	RA  is 0-360 radians

        r = sqrt(xyz(1)**2+xyz(2)**2+xyz(3)**2)

        ara1 = xyz(1)/r
        ara2 = xyz(2)/r
        adec = xyz(3)/r
                
        if (adec + epsilon .gt. 1.0D0) then
           dec = real(halfpi)
        else if (adec - epsilon .lt. -1.0D0) then
           dec = -real(halfpi)
        else
           dec = real(asin(adec))
        end if

        if (abs(ara1).gt.epsilon.or.abs(ara2).gt.epsilon) then
           ra = real(atan2(ara2, ara1))
        else 
           ra = 0.0
        end if

	if (ra .lt. 0.0) ra = real(twopi) + ra

	return
	end


