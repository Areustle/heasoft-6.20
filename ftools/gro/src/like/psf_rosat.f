C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      double precision function psf_rosat( ierr, e, sigma, rc, fn1, fn2,
c     +  fn3, bk1, bk2, alpha, x )
c
c  $Id: psf_rosat.f,v 1.2 2013/05/21 19:08:26 irby Exp $
c
cc    Function psf_rosat - calculate normalized, on-axis PSF for ROSAT PSPC.
c
ca    Author - G. Hasinger        24 Feb 1992
cu    revised - Eric M. Schlegel  12 Aug 1992
cu        split up for efficiency
cu        changed coefficients to work in pixel space
c
ct    Status:  
c
cd    Description:  PSPC on-axis PSF is a sum of three terms:
cd
cd    1. gaussian for intrinsic PSPC resolution due to incoherent statistics
cd       of primary electron generation.
cd
cd    2. exponential due to finite penetration depth of x-rays in the 
cd       counter gas; the PSPC is focussed for 1keV x-rays, so chromatic
cd       aberration increases with energy
cd
cd    3. lorentz function for the mirror scattering; theoretically, the
cd       behavior increases as the square of the energy if the grazing 
cd       angle is constant, so diffraction forces the shape parameters to
cd       vary as inverse energy.
cd
cd    Technically, these 3 components should be folded together, but their
cd    angular domains are sufficiently separated that simply adding them
cd    together is reasonably accurate.  Detailed comparison with calibration
cd    data at monochromatic energies of 0.28, 0.93, 1.49, and 1.70keV provide
cd    the parameter values.  No fit is possible below 0.15keV due to the
cd    PSPC electronics.
c
cp    Call:   psf_rosat( e, sigma, rc, fn1, fn2, fn3, bk1, bk2,
cp                alpha, x, ier )
cp
cp    Input:
cp      e       real       energy( keV)
cp      sigma .. alpha     output from psf_scal
cp      x       real       angle of pixel from on-axis (arcsec)
cp    Output:
cp      ier     integer    0 = no error
cp                         1 = energy out of bounds (0.15 - 2.5keV)
cp      PSF     real       PSF, normalized
c
c
c
C  $Log: psf_rosat.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.3  1996/07/31  19:21:35  jae
c *** empty log message ***
c
c Revision 5.2  1996/07/31  19:20:10  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:52:22  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:41  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      double precision function psf_rosat( ierr, e, sigma, rc, fn1, fn2,
     +  fn3, bk1, bk2, alpha, x )
	character(80) id
	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/errrep.copy'

        save

	common /id/id
c     implicit none
      integer ierr
      double precision e, sigma, rc, fn1, fn2, fn3, bk1, bk2, alpha,
     +  x, arg1, arg2
c
	id = '$Id: psf_rosat.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
	LOC='PSF_ROSAT'
	if(jae_psf)write(*,'("In routine ",a)') LOC
      ierr = 0
      if( ( e .ge. 0.15 ) .and. ( e .le. 2.5 ) ) then
c
c       Calculate function.
c
        arg1 = 0.5 * ( x / sigma ) ** 2
        if( arg1 .ge. 75.0 ) arg1 = 75.0
        arg2 = x / rc
        if( arg2 .ge. 75.0 ) arg2 = 75.0
        psf_rosat = fn1 * exp( -arg1 ) + fn2 * exp( -arg2 )
        if( x .le. bk2 ) then
          psf_rosat = psf_rosat + ( fn3 / ( bk1 ** 2 + x ** 2 ) )
        else
          psf_rosat = psf_rosat + ( fn3 / ( bk1 ** 2 + bk2 ** 2 ) ) * 
     +                  ( x / bk2 ) ** ( -alpha )
        endif
      else
        psf_rosat = 0.0
        ierr = 1
      endif
c
      return
      end
c
