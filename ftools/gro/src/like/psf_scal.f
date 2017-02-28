C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      subroutine psf_scal( outopt, ierr, e, offax, sigma, rc, fn1, fn2,
c     +   fn3, bk1, bk2, alpha )
c
c  $Id: psf_scal.f,v 1.2 2013/05/21 19:08:26 irby Exp $
c
cc    subroutine psf_scal - calculate scale factors and normalizations
cc    for off-axis PSPC PSF.
c
ca    Author - G. Hasinger        24 Feb 1992
cu    revised - Eric M. Schlegel  12 Aug 1992
cu        split up for efficiency
cu    revised - Eric M. Schlegel  14 Apr 1993 
cu        add off-axis PSF
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
cd    The off-axis blur of the telescope, although highly structured and
cd    asymmetric, can be modeled by a simple Gaussian in its radially
cd    integrated profile.  This Gaussian is added in quadrature to the
cd    Gaussian of the detector.  Since the PSF is not convolved, but a 
cd    simple addition of terms, the contribution of the exponential term
cd    must be diminished while the Gaussian is "eating up" the exponential.
cd    This is modelled as a Gaussian decay of the exponential term as a 
cd    function of the off-axis angle.
cp
cp    Input:
cp      e       dbl prec   energy( keV)
cp      offax   dbl prec   off-axis angle (arc min)
cp    Output:
cp      sigma   dbl prec   gaussian sigma - mirror and detector in quadrature
cp      rc      dbl prec   exponential scale factor
cp      fn1     dbl prec   normalization, gaussian
cp      fn2     dbl prec   normalization, exponential
cp      fn3     dbl prec   normalization, Lorentz
cp      bk1     dbl prec   Lorentz scale factor, low energies
cp      bk2     dbl prec   power law scale factor, high energies
cp      alpha   dbl prec   power law exponent
cp      ier     integer    0 = no error
cp                         1 = energy out of bounds (0.15 - 2.5keV)
cp
cp    In the code below, the "^^^^^" or "vvvvvvv" indicate a portion of the code
cp    which has been changed.  Usually, the changes have been made to convert the
cp    coefficients from arcsec to pixels (0".5 arcsec pixels in the PSPC).  A
cp    few of the changes indicate where the off-axis PSF is different from the
cp    on-axis PSF, or where an off-axis PSF coefficient has been adjusted to 
cp    match the data better, such as in the scattering coefficient.
cp                 -- Eric M. Schlegel
C
C
C  $Log: psf_scal.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:29:54  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:52:23  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:43  jae
c Subroutine Module for like V5.00
c
C
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine psf_scal( outopt, ierr, e, offax, sigma, rc, fn1, fn2,
     +   fn3, bk1, bk2, alpha )
	character(80) id
	common /id/id
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'

        save

c     implicit none
      integer ierr, outopt
      double precision e, offax, sigdet2, sigmir2, sigma, rc, fn1, fn2,
     +    fn3, bk1, bk2, alpha, a1, a2, a3, pi, aux
      parameter( pi = 3.1415927 )
c
	id = '$Id: psf_scal.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
	LOC='PSF_SCAL'
	if(jae_psf_scal)write(*,'("In routine ",a)') LOC
 9000 format(/,' Psf_scal exits.')
c
      ierr = 0
      if( ( e .ge. 0.15 ) .and. ( e .le. 2.5 ) ) then
c
c       Calculate energy-dependent parameters.
c
c       Exponential fraction
c
c       on-axis form:  a2 = 10 ** ( -1.618 + 0.507 * e + 0.148 * e ** 2 )
c
        a2 = 10**(-1.635 +0.639 * e + 0.052 * e * e ) * 
     +      exp( -0.5 * (offax / 12.0 ) ** 2 )
c
c       Scattering fraction:  original coefficient = 0.059
c
        a3 = 0.075 * e ** 1.43
c
c       Cure the exponential artifact.
c
        a2 = min( a2, 1.0 - a3 )
c
c       Gaussian fraction (total integral under the PSF is 1.0)
c
        a1 = 1.0 - a2 - a3
c
c       Gaussian sigma - Detector (units = pixels)
c
c                vvvvv                     vvvvv
c
        sigdet2 = 434.8 * e ** ( -0.888 ) + 4.484 * e ** 6
c
c       Gaussian sigma - Mirror (units = pixels)
c
c                vvvvv
c
        sigmir2 = 0.876 * offax ** 2.848
c
        sigma = sqrt( sigdet2 + sigmir2 )
c
c       Exponential scale factor (units = pixels)
c
c                  vvvvvv                   vvvv
c
        rc = sqrt( 202.44 * e ** (-1.472) + 27.2 * e ** 5.62 )
c
c       Lorentz scale factors (units = pixels):  behavior breaks at 
c       high energy.
c
c             vvvvv
c
        bk1 = 79.9 / e
        bk2 = 1723.8 / e
c
c       Power-law slope which alters Lorentz at high energy
c
        alpha = 2.119 + 0.212 * e
c
c       Normalizations (integrate from 0 to infinity)
c
        fn1 = a1 / ( 2.0 * pi * sigma **2 )
        fn2 = a2 / ( 2.0 * pi * rc ** 2 )
        aux = bk2 * bk2 / bk1 / bk1
        fn3 = a3 / ( pi * ( log( 1.0 + aux ) + 
     +    ( 2.0 * aux ) / ( ( 1.0 +  aux ) * (alpha - 2.0 ) ) ) ) 
      else
        ierr = 1
      endif
c
      if( outopt .ge. 3 ) print 9000
c
      return
      end
c
