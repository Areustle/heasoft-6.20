CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(BINANG) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  BINANG
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: binang.f,v 1.3 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Computes the polar and azimuth angles for each bin of the
CH1            map every time the pointing direction changes. Also
CH1            computes the solid angle of each bin the first time the
CH1            routine is called.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call BINANG(dir,polang,azmang,solidn)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     dir(4)      R*4     I   Current pointing direction of instrument
CH2                             (+Z-RA, +ZDC, +X-RA, +X-DC)
CH2     polang      R*4     O   Polar angle for all bins of the map
CH2     (594,200)
CH2     azmang      R*4     O   Azimuth angle for all bins of the map
CH2     (594,200)
CH2     solidn(200) R*4     O   Solid angle (depends on 1 dimension)
CH2
CH2  Called by:  EXPOSR
CH2
CH2  Calls:
CH2   CELGAL: Transforms coordinates between celestial and galactic
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
CH3  az         R*4        -      Pointing dir Z-RA or longit (Alpha Z)
CH3  dz         R*4        -      Pointing dir Z-DC or latit (Delta Z)
CH3  ax         R*4        -      Pointing dir X-RA or longit (Alpha X)
CH3  dx         R*4        -      Pointing dir X-DC or latit (Delta X)
CH3  as         R*4        -      Source RA or longitude (Alpha S)
CH3  ds         R*4        -      Source DEC or latitude (Delta S)
CH3  oldr1      R*4       1e75    Old value of pointing dir, dir(1)
CH3  oldr2      R*4       1e75    Old value of pointing dir, dir(2)
CH3  oldr3      R*4       1e75    Old value of pointing dir, dir(3)
CH3  oldr4      R*4       1e75    Old value of pointing dir, dir(4)
CH3  p5         R*4        -      0.5 degrees in radians
CH3  crvl1      R*4        -      Coord of x reference point in radians
CH3  cdel1      R*4        -      Increment of x coordinate in radians
CH3  crvl2      R*4        -      Coord of y reference point in radians
CH3  cdel2      R*4        -      Increment of y coordinate in radians
CH3  g          I*4        -      FITS data group number (for polar map)
CH3  first      L*4        T      First time routine called flag
CH3
CH4  Logical Units Used: None
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    If (first time) compute values for conversion
CH4    If (instrument pointing dir not changed by > 0.5 degrees) return
CH4    If (coodinate system is galactic) convert pointing dir to galacti
CH4    If (the grid is rectangular or Aitoff) then
CH4       Compute the bin coordinate on the 2nd axis
CH4       For (j=1 to number of bins on 2nd axis) do
CH4          Compute the bin coordinate on the 1st axis
CH4          If (first time) compute solid angle from 2nd axis coord
CH4          For (i=1 to number of bins on 1st axis) do
CH4             Compute the bin polar angle
CH4             Compute the bin azimuth angle
CH4             Increment the bin coordinate on the 1st axis
CH4          End for
CH4          Increment the bin coordinate on the 2nd axis
CH4       End for
CH4    Else if (the grid is polar) then
CH4       For (i=1 to number of bins on polar axis) do
CH4          If (i corresponds to a new group) then
CH4             Increment the group index
CH4             Reset the azimuth index
CH4             Compute the bin increment on polar axis for this group
CH4             Compute the bin increment on azimuth axis for this group
CH4             Compute the polar bin coordinate for i
CH4          End if
CH4          Compute the first azimuth bin coordinate
CH4          For (j=1 to the number of azimuth bins at bin i) do
CH4             If (first time) compute the solid angle for i
CH4             Compute the bin polar angle
CH4             Compute the bin azimuth angle
CH4             Increment the bin coordinate on the 1st axis
CH4          End for
CH4       End for
CH4    End if
CH4  End BINANG
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.0	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'
CH5				Changed max data value from 1e75 to 1e38.
CH5				Corrected case where azimuth angles are
CH5				> 2 pi or < 0.
CH5 $Log: binang.f,v $
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
c Revision 2.7  1995/09/26  20:21:50  nancy
c Added a call to a new routine, WTDFACTR that generates a spectrum-weighted
c scale factor to apply for wide energy ranges without changing the standard
c energy bin values.  The change is needed to implement circumstances where
c scale factor varies with energy.  Heretofore it has been independent of
c energy.
c
c Revision 2.6  1992/12/14  20:13:19  albert
c Minor modification to use either 0 to 360 or -180 to 180 range.
c
c Revision 2.5  1992/10/14  15:17:47  albert
c Multiplied the sensitivity scaling factor by the solid angle the first time
c through so as to scale the exposure by the user supplied factors.
c
c Revision 2.4  1992/04/08  14:57:34  albert
c Changed polar angles and azimuth angles arrays to be variable dimensioned
c as the bin data arrays are.
c
c Revision 2.3  1992/04/01  21:09:17  albert
c Used variable dimension arrays. Computed polar angles using bin center
c coordinates.
c
c Revision 2.2  1991/12/02  20:20:20  albert
c Changed the computation of the bins solid angles to allow for rectangular
c bins.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CH5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine BINANG(dir,polang,azmang,solidn)

      real      dir(4),az,dz,ax,dx,oldr1,oldr2,oldr3,oldr4,twopi,conv,p5
Cae   real      crvl1,cdel1,crvl2,cdel2,polang(594,200),azmang(594,200)
Cae   real      as,ds,az1,dz1,ax1,dx1,solidn(200)
      real      crvl1,cdel1,crvl2,cdel2,oaz,odz
      real      as,ds,az1,dz1,ax1,dx1,crv1,crv2,wtdfac(10)
      integer   i,j,k,n,g,iret
      logical   first

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'
      real    polang(naxis1,naxis2),azmang(naxis1,naxis2)
      real    solidn(naxis2,naxis3)

      save

Cesp  ! 1e75 is too large for real on the SUN !
Cesp  data oldr1/1e75/,oldr2/1e75/,oldr3/1e75/,oldr4/1e75/,first/.true./
      data oldr1/1e38/,oldr2/1e38/,oldr3/1e38/,oldr4/1e38/,first/.true./

      character(80)	id
      common	/id/id
      id = '$Id: binang.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

C---> First time, convert degrees to radians
      if (first) then
         twopi = ATAN(1.0) * 8
         conv  = twopi/360.0
         p5    = 0.5 * conv
         crvl1 = crval1 * conv
         cdel1 = cdelt1 * conv
         crvl2 = crval2 * conv
         cdel2 = cdelt2 * conv
      endif

C---> First time, compute effective scale factors for performance
      if (first) call wtdfactr(wtdfac,iret)

C---> Nothing done if pointing direction has not changed by > .5 degrees
      if (abs(oldr1-dir(1)).le.p5 .and. abs(oldr2-dir(2)).le.p5 .and.
     &    abs(oldr3-dir(3)).le.p5 .and. abs(oldr4-dir(4)).le.p5) return

C---> Put pointing direction in 0-2pi range
      az = dir(1)
      dz = dir(2)
      ax = dir(3)
      dx = dir(4)
      if (az.lt.0.0)   az = az + twopi
      if (dz.gt.twopi) dz = dz - twopi
      if (ax.lt.0.0)   ax = ax + twopi
      if (dx.gt.twopi) dx = dx - twopi

C---> Convert pointing direction to galactic coordinates if necessary
      if (coords .eq. 'GALA') then
         az1 = az
         dz1 = dz
         ax1 = ax
         dx1 = dx
         call CELGAL('CG',az1,dz1,az,dz,iret)
         call CELGAL('CG',ax1,dx1,ax,dx,iret)
      endif
      oaz = az
      odz = dz

C---> Do the rectangular or Aitoff case
      if (gridtp.eq.'RECT'.or.gridtp.eq.'AITF') then
	 crv1 = crvl1
	 if (crv1 .lt. 0.0 .and. crv1+twopi .le. az) crv1 = crv1 + twopi
	 az = crv1 + (int((az-crv1)/cdel1+crpix1)-crpix1+0.5)*cdel1
	 crv2 = crvl2
	 if (crv2 .gt. twopi) crv2 = crv2 - twopi
	 dz = crv2 + (int((dz-crv2)/cdel2+crpix2)-crpix2+0.5)*cdel2
         ds = crvl2 + (crpix2-0.5)*cdel2

	 if (abs(oaz-az).gt.cdel1/2) print*,'ERROR 1 in BINANG',az,oaz
	 if (abs(odz-dz).gt.cdel1/2) print*,'ERROR 2 in BINANG',dz,odz

C------> Loop over the Y axis
         do j=1,naxis2
            n = naxis1
            if (gridtp.eq.'AITF') then
               n = naxs12(j)
               cdel1 = twopi/n
            end if
            as = crvl1 + (crpix1-0.5)*cdel1
            if (first) then
	       do k=1,naxis3
		  solidn(j,k) = cdel1*cdel2 * COS(ds) * wtdfac(k)
	       enddo
	    endif

C---------> Loop over the X axis and compute the angles
            do i=1,n
               if (as.lt.0.0)   as = as + twopi
               if (as.gt.twopi) as = as - twopi
               polang(i,j) = ACOS( SIN(dz)*SIN(ds) +
     &                             COS(dz)*COS(ds)*COS(as-az) )
               azmang(i,j) = twopi +
     &                      ATAN2( COS(dx)*COS(dz)*SIN(ax-az),SIN(dx))-
     &                      ATAN2( COS(ds)*COS(dz)*SIN(as-az),
     &                              SIN(ds)-SIN(dz)*COS(polang(i,j)) )
	       if (azmang(i,j).lt.0.0) azmang(i,j) = azmang(i,j) + twopi
	       if (azmang(i,j).gt.twopi) azmang(i,j) = azmang(i,j)-twopi
               as = as + cdel1
            enddo
            ds = ds + cdel2
         enddo

C---> Do the polar map
      else if (gridtp.eq.'POLA') then
         n = 0
         g = 0

C------> Loop over the polar axis
         do i=1,naxis1

C---------> Compute the bin coordinates from the group information
            if (g.eq.0 .or. n.ge.ftparm(1,g)) then
               g = g + 1
               n = 0
               cdel1 = ftparm(4,g)*conv
               cdel2 = ftparm(5,g)*conv
               as = ftparm(2,g)*conv + cdel1/2.0
            endif
            ds = ftparm(3,g)*conv + cdel2/2.0
            if (first) then
	       do k=1,naxis3
		  solidn(i,k) = cdel1*cdel2 * COS(as) * wtdfac(k)
	       enddo
	    endif

C---------> Loop over the azimuth axis and compute the angles
            do j=1,naxs12(i)
               polang(i,j) = ACOS( SIN(dz)*SIN(ds) +
     &                             COS(dz)*COS(ds)*COS(as-az) )
               azmang(i,j) = twopi +
     &                       ATAN2( COS(dx)*COS(dz)*SIN(ax-az),
     &                              SIN(dx)) -
     &                       ATAN2( COS(ds)*COS(dz)*SIN(as-az),
     &                              SIN(ds)-SIN(dz)*COS(polang(i,j)))
               n = n + 1
               ds = ds + cdel2
            end do
            as = as + cdel1
         end do
      endif

C---> Save the current pointing direction
      oldr1 = dir(1)
      oldr2 = dir(2)
      oldr3 = dir(3)
      oldr4 = dir(4)
      first = .false.

      return

CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(BINANG) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
