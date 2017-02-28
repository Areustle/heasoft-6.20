CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(INTENS) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  INTENS
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: intens.f,v 1.3 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Generates the intensity map from the exposure and counts
CH1            maps.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call INTENS
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  Called by:  INTMAP
CH2
CH2  Calls:  None
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
CH3  maxv(2)    R*4       0.0     Maximum exposure (1) & intensity (2)
CH3  exgbin     R*4        -      Exposure bins for galactic disk maps
CH3  (594,66,10)
CH3  ingbin     R*4        -      Intensity bins for galactic disk maps
CH3  (594,66,10)
CH3  cngbin     R*4        -      Counts bins for galactic disk maps
CH3  (594,66,10)
CH3  maxint     I*4        -      Maximum I*2 or I*4 integer
CH3  maxi4      I*4   2147483647  Maximum I*4 integer
CH3  maxi2      I*4      32767    Maximum I*2 integer
CH3  type       Ch*9       -      Data type ('EXPOSURE' or 'INTENSITY')
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       6              Printer report
CH4
CH4  Method:
CH4    For (k=1 to number of energy levels) do
CH4       If (the grid type is rectangular or Aitoff) then
CH4          For (j=1 to the number of bins on the 2nd axis) do
CH4             If (grid type is Aitoff) get number of bins on axis 1
CH4             For (i=1 to the number of bins on axis 1) do
CH4                Intensity bin = counts bin / exposure bin
CH4                If (this exposure bin > max exposure bin)save new max
CH4                If (intensity bin > max intensity bin) save new max
CH4             End for
CH4          End for
CH4       Else if (the grid type is polar) then
CH4          For (i=1 to the number of bins on the axis 1) do
CH4             For (j=1 to the number of bins on axis 2) do
CH4                Intensity bin = counts bin / exposure bin
CH4                If (this exposure bin > max exposure bin)save new max
CH4                If (intensity bin > max intensity bin) save new max
CH4             End for
CH4          End for
CH4       End if
CH4    End for
CH4    Compute the scaling factor for the exposure data
CH4    Compute the scaling factor for the intensity data
CH4  End INTENS
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Changed ! comments to C comments.
CH5				Merged changes from version 1.03 on IBM.
CH5				Computed minimum non-zero bin
CH5                             value for intensity and exposure maps.
CH5
CH5 $Log: intens.f,v $
CH5 Revision 1.3  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:03  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.5  2001/02/27  15:51:40  dlb
c Modified to avoid anomalously small exposure values from
c producing bins with correspondingly large intensities.
c These were giving rise to such large intensity bscale
c values that almost all of the intensity FITS map bins were
c empty (values below 1 were truncated to zero when stored.
c
c Revision 2.4  1994/09/26  19:29:24  albert
c Modified to terminate in error if all exposure or intensity bins are 0.
c
c Revision 2.3  1994/03/16  20:16:07  albert
c Corrected misplaced (but unused) continuation character
c
c Revision 2.2  1992/04/01  21:30:24  albert
c Used variable dimension arrays for the bin data obtained as subroutine
c parameters. Removed previous special processing for galactic disk maps
c which could not be stored in the old array dimension.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine INTENS(cntbin,expbin,intbin)

      real      expbin(naxis1,naxis2,naxis3)
      real      intbin(naxis1,naxis2,naxis3)
      integer   cntbin(naxis1,naxis2,naxis3)
Cae   real      maxv(2),minv(2),exgbin(594,66,10),ingbin(594,66,10)
      real      maxv(2),minv(2)
Cdlb
      real      expmax, expsf
Cdlb
Cae   integer   maxint,maxi4,maxi2,i,j,k,n,cngbin(594,66,10)
      integer   maxint,maxi4,maxi2,i,j,k,n
      character type*9

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'

      save

Cae   equivalence (cntbin(1,1,1),cngbin(1,1,1))
Cae   equivalence (expbin(1,1,1),exgbin(1,1,1))
Cae   equivalence (intbin(1,1,1),ingbin(1,1,1))
      data maxv /0.0, 0.0/, minv /1e36, 1e36/

Cesp  ! F77 1.2 does not implement ! comments.
Cesp  data maxi4  /2147483647/           ! maximum bin value in I*4
Cesp  data maxi2  /32767/                ! maximum bin value in I*2
Cesp  ! maximum bin value in I*4
      data maxi4  /2147483647/
Cesp  ! maximum bin value in I*2
      data maxi2  /32767/

      character(80)	id
      common	/id/id
      id = '$Id: intens.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

Cdlb--->  Added code to find the exposure scale factor before generating
Cdlb      the intensities in order to truncate anomalously low values
Cdlb      to zero.  These were causing occasional large bscale values
Cdlb      in the intensity that resulted in blank maps.

     
      expmax = 0.0
      do k=1,naxis3
         do j=1,naxis2
            do i=1,naxis1
               if( expbin(i,j,k).gt.expmax ) expmax = expbin(i,j,k)
            enddo
         enddo
      enddo
      expsf = 32767/expmax

Cdlb      End addition


C---> Loop over all energy levels
      do k=1,naxis3

C------> Compute the rectangular or Aitoff map bins
         if (gridtp.eq.'RECT' .or. gridtp.eq.'AITF') then

C---------> Loop over the Y or latitude axis
            do j=1,naxis2
               n = naxis1
               if (gridtp.eq.'AITF') n = naxs12(j)

C------------> Do the maps that have less than 200 bins on X
Cae            if (naxis1.le.200) then

Cesp		  ! F77 1.2 does not implement ! comments.
Cesp              do i=1,n                                  ! X axis
Cesp		  ! X axis
               do i=1,n
Cdlb                 if (expbin(i,j,k).ne.0) then
Cdlb                    Test to be sure exposure will not truncate
Cdlb                    to zero when stored.

                  if (expsf*expbin(i,j,k) .ge. 1.0 ) then
                     intbin(i,j,k) = cntbin(i,j,k)/expbin(i,j,k)
Cae			if (cntbin(i,j,k) .ne. 0)
Cae  +				print *,cntbin(i,j,k),expbin(i,j,k),
Cae  +				intbin(i,j,k),i,j,k
                  else
                     intbin(i,j,k) = 0.0
                  endif

C------------------> Find the maximum exposure and intensity bins
                  if (expbin(i,j,k).gt.maxv(1))
     &                 maxv(1) = expbin(i,j,k)
                  if (intbin(i,j,k).gt.maxv(2))
     &                 maxv(2) = intbin(i,j,k)
                  if (expbin(i,j,k).lt.minv(1).and.expbin(i,j,k)
     &                 .ne.0) minv(1) = expbin(i,j,k)
                  if (intbin(i,j,k).lt.minv(2).and.intbin(i,j,k)
     &                 .ne.0) minv(2) = intbin(i,j,k)
               enddo

C------------> Do the galactic disk maps
Cae            else
Cesp		  ! F77 1.2 does not implement ! comments.
Cesp              do i=1,n                                  ! X axis
Cesp		  ! X axis
Cae                  do i=1,n
Cae                     if (exgbin(i,j,k).ne.0) then
Cae                        ingbin(i,j,k) = cngbin(i,j,k)/exgbin(i,j,k)
Cae                     else
Cae                        ingbin(i,j,k) = 0.0
Cae                     end if
Cae
C------------------> Find the maximum exposure and intensity bins
Cae                     if (exgbin(i,j,k).gt.maxv(1))
Cae     &                  maxv(1) = exgbin(i,j,k)
Cae                     if (ingbin(i,j,k).gt.maxv(2))
Cae   &                  maxv(2) = ingbin(i,j,k)
Cae		     if (exgbin(i,j,k).lt.minv(1).and.exgbin(i,j,k)
Cae     &                  .ne.0) minv(1) = exgbin(i,j,k)
Cae		     if (ingbin(i,j,k).lt.minv(2).and.ingbin(i,j,k)
Cae     &                  .ne.0) minv(2) = ingbin(i,j,k)
Cae                  end do
Cae               endif
   
            enddo

C------> Do the polar maps
         else if (gridtp.eq.'POLA') then

C---------> Loop over the polar angles
            do i=1,naxis1

C------------> Loop over the azimuth angles
               do j=1,naxs12(i)
                  if (expbin(i,j,k).ne.0) then
                     intbin(i,j,k) = cntbin(i,j,k)/expbin(i,j,k)
                  else
                     intbin(i,j,k) = 0.0
                  endif

C---------------> Find the maximum exposure and intensity bins
                  if (expbin(i,j,k).gt.maxv(1)) maxv(1) = expbin(i,j,k)
                  if (intbin(i,j,k).gt.maxv(2)) maxv(2) = expbin(i,j,k)
		  if (expbin(i,j,k).lt.minv(1).and.expbin(i,j,k).ne.0)
     &                 minv(1) = expbin(i,j,k)
		  if (intbin(i,j,k).lt.minv(2).and.intbin(i,j,k).ne.0)
     &                 minv(2) = intbin(i,j,k)
               enddo
            enddo
         endif
      enddo
      
C---> Compute the scaling factor for the exposure and intensity maps
c     if (bitpix .eq. 32) maxint = maxi4
c     if (bitpix .eq. 16) maxint = maxi2
      maxint = maxi2
      type = 'EXPOSURE'
      write(6,'(/,''Data Type     Scale          Max        Min '')')

      do i=2,3
c        bscale(i) = (maxv(i-1) - minv(i-1)) / maxint
c        bzero(i)  = minv(i-1)
c        blank(i)  = -bzero(i)/bscale(i)
         bscale(i) = maxv(i-1) / maxint
         bzero(i)  = 0.0
	 blank(i) = 0
         if (bscale(i).eq.0) then
            bscale(i) = 1.0
            write(6,*) 'ERROR: all bins in the ',type,' map are 0'
	    retcod = 1
         endif
	 write(6,*) type,' ',bscale(i),maxv(i-1),minv(i-1)
         type = 'INTENSITY'
      enddo

      return

CCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(INTENS) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
