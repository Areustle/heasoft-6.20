CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(WTDFACTR) CCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  WTDFACTR
CH1
CH6  Version: 1.01                  Date: 11/09/00
CH1  Version: 1.00                  Date: 08/03/95
CH1  $Id: wtdfactr.f,v 1.3 2013/05/21 19:08:25 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Dave Bertsch            - 08/03/95
CH1
CH1  Function: Computes the weighted scale factor for long term
CH1            instrument performance for non-standard energy bins.
CH1            The scale factor is used in the routine BINANG.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language  SUN - FORTRAN
CH1
CH2  Calling Sequence:  Call WTDFACTR(wtdfac)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     wtdfac(10)  R*4     O   Weighted scale factor for up to 10
CH2                             energy intervals.
CH2     iret        I*4     O   Return code.  Normally 0.  Set to 1
CH2                             if weighted factor is 0.  In this case
CH2                             wtdfac is set to 1.00.
CH2
CH2  Called by:  BINANG
CH2
CH2  Calls:
CH2   None
CH2
CH3  COMMON Use:
CH3  COMMON Block Name: FITSDT (Holds the FITS file variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 bitpix     Integer   Number of bits per pixels
CH3 naxis      Integer   Number of axis in the map
CH3 naxis1     Integer   Number of bins on the 1st axis
CH3 naxis2     Integer   Number of bins on the 2nd axis
CH3 naxis3     Integer   Number of bins on the 3rd axis
CH3 bscale(3)  Real      Bin scaling factor (counts, exposure, intensty)
CH3 bzero(3)   Real      Bin offset value (counts, exposure, intensty)
CH3 ftparm     Real*4    5 parameters (200 groups). index 1 to 5 are:
CH3   (5,200)            1:number bins in group,   2:position on axis1,
CH3                      3:position on axis2,      4:increment on axis1,
CH3                      5:increment on axis 2
CH3 gridtp     Ch*4      Grid type ('RECT', 'POLA' or 'AITF')
CH3 headpf(2)  Real      Two pointers for header buffer
CH3 evclas     Integer   Event class
CH3 energy     Real      Energy level ranges
CH3  (2,10)
CH3 pcount     Integer   Number of parameters in FITS file
CH3 gcount     Integer   Number of groups in FITS data
CH3 naxs12(200)Integer   Number of bins on axis with variable # of bins
CH3 crval1     Real      Coordinate of reference point on axis 1
CH3 crpix1     Real      Array index of reference point on axis 1
CH3 cdelt1     Real      Increment of coordinate along axis 1
CH3 crval2     Real      Coordinate of reference point on axis 2
CH3 crpix2     Real      Array index of reference point on axis 2
CH3 cdelt2     Real      Increment of coordinate along axis 2
CH3 coords     Ch*4      Coordinate system used ('GALA' or 'CELE')
CH3 buffer(3)  Ch*2880   FITS record buffer (may hold up to 3 header rc)
CH3 cntbin     Integer   Counts map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3 expbin     Real      Exposure map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3 intbin     Real      Intensity map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3
CH6  spec_ndx() R*4               Spectral indices for a piecewise
CH6                               power law spectral fit to the 
CH6                               predominant diffuse spectrum
CH6  Fspec()    R*4               Spectral coefficients/(1-spec_ndx)
CH6                               for the piecewise power-law spectrum.
CH3  first      L*4        T      First time routine called flag
CH3  imin       I*4               Index of the standard energy interval
CH3                               where lower energy bound begins
CH3  Imax
CH3  e1         R*4               Index of the standard energy interval
CH3  e2         R*4               where upper energy bound ends
CH3  wt         R*4               The spectral weight for a specific
CH3                               energy region
CH3  sum_wt     R*4               The accumulated sum of the weights for
CH3                               the different standard bins within the
CH3                               range of energies
CH3  sumfwt     R*4               The accumulated weight divided by the
CH3                               scale factor for the standard intervals,
CH3                               (proportional to the corrected intensity)
CH3  wtdfac()   R*4               The output weighted scale factors for
CH3                               each energy bin.
CH3
CH3
CH4  Logical Units Used: None
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    For (j=1 to number of energy ranges) do
CH4       If(energy range is standard) then
CH4           Set the weighted factor to be the standard scale factor
CH4       Else
CH4           Compute intensity in each standard energy bin or fraction
CH4           of bin by piecewise integration of an energy power law
CH4           Compute intensity in each standard energy bin or fraction
CH4           of bin, corrected by the standard scale factors
CH4           Form the ratio of the two intensities to obtain the
CH4           weighted factor.  Use fixed spectral indices appropriate
CH4           for diffuse emission.
CH4       End if
CH4    End For
CH4    Restrict the range of the weighted scale factor to within
CH4    0.2 and 1.1
CH4    If (sum weighted factors is zero)
CH4       Set factor to 1.0 and return code to 1
CH4    End if
CH4
CH$  End WTDFACTR
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5
CH6     1.      D.L.Bertsch  11/09/00  Version 1.01.  Changes as follows:
CH6
CH6  Made a slight change in the weighting scheme that will be used 
CH6  for viewing periods 4280 and later (last full gas fill.)  This 
CH6  scheme correctly uses piecewise spectral coefficients.  The
CH6  same result will be obtained for data prior to VP4280.  The
CH6  difference between the old and new scheme is slight.
CH6
CH5 $Log: wtdfactr.f,v $
CH5 Revision 1.3  2013/05/21 19:08:25  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.2  2002/04/18 19:37:57  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:04  irby
CH5 New GRO tool intmap.
CH5
c Revision 1.3  2001/06/22  19:07:14  dlb
c Extended the upper range value on energy so that the last scale factor
c would apply to energies above 10 GeV.
c
c Revision 1.2  2000/12/18  20:50:24  dlb
c Slight modification to the weighting scheme for view periods after 4270.
c No change for earlier viewing periods.
c
c Revision 1.1  1995/09/27  17:09:32  programs
c Initial revision
c
CH5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Subroutine WTDFACTR(wtdfac,iret)

      real      wtdfac(10), stdbnd(11), wt, e2, e1, sexp, 
     &          Fspec(10), spec_ndx(10),ratio
      real*8    sum_wt, sum_fwt
      integer   i,n, imin, imax, iret

      logical   first

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'


      character(80)      id

      save

      common    /id/id

C==>  Standard energy bins and specral indices of the diffuse emission
*     that is used to determine the >100 MeV performance factors.

      data stdbnd/ 30., 50., 70., 100., 150., 300., 500., 1000., 2000.,
     &             4000., 999999. /
      data spec_ndx/ 1.7, 1.7, 1.7, 1.7, 1.7, 1.9, 1.9, 2.4, 2.4, 2.4/

      data first/.true./
C==>

      id = '$Id: wtdfactr.f,v 1.3 2013/05/21 19:08:25 irby Exp $'

C==>   Generate the factors for the integral spectrum used in weighting.
*     The first factor is arbitarily 1.0 (used in a ratio) and the 
*     rest are set to make the spectrum piecewise continuous.
       

      if( first ) then
         Fspec(1) = 1.0
         do i = 1, 9
            Fspec(i+1) = Fspec(i)*(1.0-spec_ndx(i))/(1.0-spec_ndx(i+1))
     &           *stdbnd(i+1)**(spec_ndx(i+1)-spec_ndx(i))
         end do
         first = .false.
      endif


      iret = 0

      do n = 1, naxis3
C==>       Find the relevant standard energy regions and compute the
*          sums of unweighted and weighted relative intensity

         imin = 1
         imax = 1

         do i = 1, 10
            if( stdbnd(i) .le. energy(1,n) )  imin = i
            if( stdbnd(i+1) .lt. energy(2,n)) imax = i+1
         end do

         imax = min( imax,10 )
         
         sum_wt = 0.0
         sum_fwt = 0.0

         do i = imin,imax
            e2 = min( energy(2,n), stdbnd(i+1) )
            e1 = max( energy(1,n), stdbnd(i)   )
            sexp = 1.0 - max( 1.1, spec_ndx(i) )

            if( strtim.ge.9967 ) then ! after the last gas fill
               wt = Fspec(i) * (e1**sexp - e2**sexp)
               sum_fwt = sum_fwt + wt * sfact(i)
            else
               wt = e1**sexp - e2**sexp
               sum_fwt = sum_fwt + wt/max( 0.010,sfact(i) )
            endif

            sum_wt = sum_wt + wt
        end do

        if( strtim.ge.9967 ) then
           wtdfac(n) =  sum_fwt/sum_wt
        else
           if( sum_fwt .ne. 0.0 ) then
              ratio = sum_wt/sum_fwt
              wtdfac(n) = min( 2.0, max( 0.01, ratio ))
           else
              wtdfac(n) = 1.0
              iret = 1
           endif
        endif
      end do                    ! naxis loop

      return
      end
