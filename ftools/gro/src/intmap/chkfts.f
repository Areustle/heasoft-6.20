CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(CHKFTS) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  CHKFTS
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  $Id: chkfts.f,v 1.3 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1
CH1  Function: Verifies that the FITS file read conforms to the
CH1            requirements of the INTMAP program.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call CHKFTS(simple,bunit,groups,nlev,ctype1,
CH2                                 ctype2,maptyp)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     simple      L*4     I   Determines if FITS is in simple format
CH2     bunit       Ch*8    I   FITS data bin unit
CH2     groups      L*4     I   Determines if FITS data is in groups
CH2     nlev        I*4     I   Number of energy levels counted
CH2     ctype1      Ch*4    I   Coordinate type on 1st axis
CH2     ctype2      Ch*4    I   Coordinate type on 2nd axis
CH2     maptyp      Ch*4    I   Map type
CH2
CH2  Called by:  READFT
CH2
CH2  Calls: None
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
CH3  COMMON Block Name: GLOBAL (Holds the main variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 strtim     Real*8    FITS data start time (TJD & MSD combined)
CH3 endtim     Real*8    FITS data end time (TJD & MSD combined)
CH3 retcod     Integer   Program return code (0 = normal)
CH3 specin(10) Real      Spectral indexes
CH3 walldi     Integer   Wall distance (from SAGE Namelist)
CH3 maxlev     Integer   Maximum number of energy levels (10)
CH3 tascco     Integer   TASC in coincidence flag
CH3 acs        Integer   ACS value
CH3 tunit      Integer   Unit number for timeline file
CH3 eunit      Integer   Unit number for exposure history file
CH3 calfil(2)  Ch*8      Names of the calibration files
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  axlim1     I*4       200     Maximum number of bins on axis 1
CH3  axlim2     I*4       200     Maximum number of bins on axis 2
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       6              printer report
CH4
CH4  Method:
CH4     If (the map type is galactic disk) change the axis limits
CH4     If (the number of bits per pixels is not 16 or 32) then
CH4        Write an error message and set the error code
CH4     End if
CH4     If (# of axis is <> 3 or # of bins on axis 1 & 2 is < 3) then
CH4        Write an error message and set the error code
CH4     End if
CH4     If (the number of bins on any axis is not valid) then
CH4        Write an error message and set the error code
CH4     End if
CH4     If (the data type is not counts) then
CH4        Write an error message and set the error code
CH4     End if
CH4     If (the coordinate system is not galactic or celestial) then
CH4        Write an error message and set the error code
CH4     End if
CH4     If (the grid type is rectangular) then
CH4        If (the file format is not simple or groups are used) then
CH4           Write an error message and set the error code
CH4        End if
CH4     Else if (the grid type is or polar or Aitoff) then
CH4        If (the file format is simple or no groups are used) then
CH4           Write an error message and set the error code
CH4        End if
CH4     End if
CH4     If (the data time range is invalid) then
CH4        Write an error message and set the error code
CH4     End if
CH4  End CHKFTS
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga 08/29/91
CH5			Moved code from IBM to SUN.
CH5			Stripped off line numbers and trailing blanks.
CH5			Changed INCLUDE(file) to include 'file.cmn'
CH5			Allowed for case where data start time = end time.
CH5 $Log: chkfts.f,v $
CH5 Revision 1.3  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:02  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.6  2001/03/14  21:28:19  dlb
c Revised again the test for CTYPE parameters to make it compatible
c with both the new and old FITS standard.
c
c Revision 2.5  2001/03/14  19:14:03  dlb
c Changed the test for CYTPE parameters in the case of celestial
c coordinates from "RA  " to "RA--" and "DEC " to "DEC-" to
c accommodate the change in FITS standard.
c
c Revision 2.4  1994/09/26  19:28:43  albert
c Modified to test and abort if all the scaling factors are 0.
c
c Revision 2.3  1992/04/08  15:14:11  albert
c Set the maximum number of bins on the X and Y axis to 720 and 360
c respectively instead of 1800.
c
c Revision 2.2  1992/04/01  21:13:58  albert
c Used variable dimension arrays.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CH5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine CHKFTS(simple,bunit,groups,nlev,ctype1,ctype2,maptyp)

Cae   integer nlev,axlim1,axlim2
      integer nlev,i
      logical simple,groups,okflag
      character bunit*8,ctype1*4,ctype2*4,maptyp*4

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'

Cae   data axlim1/200/, axlim2/200/

      character(80)	id
      common	/id/id
      id = '$Id: chkfts.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

C---> Set axis maximum size. Galactic disk maps are wider and narrower.
Cae   if (maptyp.eq.'GALD') then
Cae      axlim1 = 594
Cae      axlim2 = 66
Cae   endif

C---> Test for valid number of bits per pixel
      if (bitpix.ne.16.and.bitpix.ne.32) then
         write(6,*) 'CHKFTS: Invalid number of bits per pixels',bitpix
         retcod = 8
      endif

C---> Test for correct axis values
      if (naxis.ne.3.or.naxis1.lt.3.or.naxis2.lt.3) then
         write(6,*) 'CHKFTS: Bad naxis,naxis1 or 2',naxis,naxis1,naxis2
         retcod = 8
      endif
Cae   if (naxis1.gt.axlim1.or.naxis2.gt.axlim2) then
Cae      write(6,*) 'CHKFTS: Bad naxis1 or naxis2',naxis1,naxis2
Cae      retcod = 8
Cae   endif
      if (naxis3.lt.1.or.naxis3.gt.maxlev.or.naxis3.ne.nlev) then
         write(6,*) 'CHKFTS: Bad naxis3 / # E levels',naxis3,nlev
         retcod = 8
      endif
      if (naxis1*naxis2*naxis3 .gt. maxbin) then
         write(6,*) 'CHKFTS: input file too large'
         write(6,*) '	     naxis1*naxis2*naxis3 >',maxbin
         retcod = 8
      endif
      if (naxis1 .gt. axl .or. naxis2 .gt. ayl) then
         write(6,*) 'CHKFTS: too many bins on one axis'
         write(6,*) '	     naxis1 or naxis2 >',axl,ayl
         retcod = 8
      endif

C---> Test that not all scale factors are null
      okflag = .false.
      do i=1, naxis3
         if (sfact(i) .ne. 0) okflag = .true.
      end do
      if (.not. okflag) then
         write(6,*) 'ERROR: all scaling factors are null'
         retcod = 8
      end if

C---> Test that the data type is event counts
      if (bunit.ne.'COUNTS') then
         write(6,*) 'CHKFTS: Bad data type ',bunit
         retcod = 8
      endif

C---> Test the coordinate system
      if (ctype1.eq.'GLON'.and.ctype2.eq.'GLAT') then
         coords = 'GALA'
      else if (ctype1(1:2).eq.'RA'.and.ctype2(1:3).eq.'DEC') then
         coords = 'CELE'
      else
         write(6,*) 'CHKFTS: bad coordinate system ',ctype1,',',ctype2
         retcod = 8
      endif

C---> Test the grid type
      if (gridtp.eq.'RECT') then
         if (.not.simple.or.groups) then
            write(6,*)'CHKFTS: rectangular grid but non-standard format'
            retcod = 8
         endif
      else if (gridtp.eq.'POLA'.or.gridtp.eq.'AITF') then
         if (simple.or..not.groups.or.pcount.lt.1.or.gcount.lt.1) then
            write(6,*) 'CHKFTS: bad grid/simple/group combination'
            retcod = 8
         endif
      else
         write(6,*) 'CHKFTS: bad grid type',gridtp
         retcod = 8
      endif

C---> Test the data time range
      if (strtim.le.0.or.endtim.le.0.or.strtim.gt.endtim) then
         write(6,*) 'CHKFTS: bad time range',strtim,endtim
         retcod = 8
      endif

      return

CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(CHKFTS) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
