CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addiff.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CH1  Routine Name:  addiff
CH1
CH1  $Id: addiff.f,v 1.7 2005/08/26 19:36:34 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Add the input map to the output map when the 2 have different
CH1	       coordinate systems.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  call addiff(in,sign,intflg)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	in	    I*4     I	Index for the current input file (1 or 2)
CH2	sign	    I*4	    I   Sign for the map operation (+1 or -1)
CH2	intflg	    I*4     I   Flag to generate the intensity & exposure maps
CH2
CH2  Called by:  addmap
CH2
CH2  Calls:
CH2   addbin_a: Adds to the current output bin
CH2   celgal_a: Converts coordinates between celestial and galactic systems
CH2   inbox : Function that determines if a given point is inside a given box
CH2   inters: Function that determines if the intersection of 2 lines is in
CH2	      between the 2 end points of each line
CH2   polyar: Function that computes the area of a polygon
CH2
CH3  COMMON Use:
CH3  COMMON Block Name: infits (Information about the input FITS file)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 icrvl1     Real*8    Position of the reference point on the first axis
CH3 icdel1     Real*8    Increment on the first axis
CH3 icrvl2     Real*8    Position of the reference point on the second axis
CH3 icdel2     Real*8    Increment on the second axis
CH3 ibitpx     Integer   Number of bits per pixel
CH3 inaxis     Integer   Number of axis in the FITS file
CH3 inaxs1     Integer   Number of bins on the first axis
CH3 inaxs2     Integer   Number of bins on the second axis
CH3 inaxs3     Integer   Number of bins on the third axis (energy levels)
CH3 ibscal(2)  Real*4    Scale to use to convert the FITS values (1 index for
CH3			 each of the 2 input maps)
CH3 ibzero(2)  Integer   Offset to use to convert the FITS values (1 index for
CH3			 each of the 2 input maps)
CH3 iftprm     Integer   FITS parameters for the Aitoff map (up to 90 groups)
CH3  (5,90)		 1: Number of elements on first axis
CH3  			 2: Bin position on first axis
CH3  			 3: Bin position on second axis
CH3  			 4: Bin incremant on first axis
CH3  			 5: Bin increment on second axis
CH3 igridt     Ch*4      Grid type ('RECT', or 'AITF')
CH3 ienrgy     Real      Energy level ranges
CH3  (2,10)
CH3 iprcnt     Integer   Number of group parameters in FITS file
CH3 igrcnt     Integer   Number of groups in FITS data
CH3 inax12(200)Integer   Number of bins on axis with variable # of bins
CH3 icrpx1     Real      Array index of reference point on axis 1
CH3 icrpx2     Real      Array index of reference point on axis 2
CH3 iznmax(10) Real      Maximum zenith angle for each energy level
CH3 iblank(2)  Real      FITS blank value (1 for each input map)
CH3 icoord     Ch*4      Coordinate system used ('GALA' or 'CELE')
CH3 ibunit(2)  Ch*8      Data type units ('COUNTS' or 'EXPOSR')
CH3 icntbn     Integer   Counts map bin values
CH3   (360,200,10)
CH3 itotcn     Integer   Total number of counts in input map
CH3 imaxvl(2)  Real      Maximum counts bin value for the input maps
CH3 iminvl(2)  Real      Minimum counts bin value for the input maps
CH3 ixmaxp(2)  Real      X position of the maximum bin value
CH3 iymaxp(2)  Real      Y position of the maximum bin value
CH3 izmaxp(2)  Integer   Z index for the maximum bin value
CH3 iexpbn     Real      Exposure map bin values
CH3   (360,200,10)
CH3
CH3  COMMON Block Name: oufits (Information about the output FITS file)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 ocrvl1     Real*8    Position of the reference point on the first axis
CH3 ocdel1     Real*8    Increment on the first axis
CH3 ocrvl2     Real*8    Position of the reference point on the second axis
CH3 ocdel2     Real*8    Increment on the second axis
CH3 obitpx     Integer   Number of bits per pixel
CH3 onaxis     Integer   Number of axis in the FITS file
CH3 onaxs1     Integer   Number of bins on the first axis
CH3 onaxs2     Integer   Number of bins on the second axis
CH3 onaxs3     Integer   Number of bins on the third axis (energy levels)
CH3 obscal(3)  Real*4    Scale to use to convert the FITS values (1 index for
CH3			 each of the 3 output maps)
CH3 obzero(3)  Integer   Offset to use to convert the FITS values (1 index for
CH3			 each of the 3 output maps)
CH3 oindex(2)  Integer   Index for the oheader buffer (2 input files)
CH3 oftprm     Integer   FITS parameters for the Aitoff map (up to 90 groups)
CH3  (5,90)		 1: Number of elements on first axis
CH3  			 2: Bin position on first axis
CH3  			 3: Bin position on second axis
CH3  			 4: Bin incremant on first axis
CH3  			 5: Bin increment on second axis
CH3 ogridt     Ch*4      Grid type ('RECT', or 'AITF')
CH3 oenrgy     Real      Energy level ranges
CH3  (2,10)
CH3 oprcnt     Integer   Number of group parameters in FITS file
CH3 ogrcnt     Integer   Number of groups in FITS data
CH3 onax12(200)Integer   Number of bins on axis with variable # of bins
CH3 ocrpx1     Real      Array index of reference point on axis 1
CH3 ocrpx2     Real      Array index of reference point on axis 2
CH3 oznmax(10) Real      Maximum zenith angle for each energy level
CH3 oblank(3)  Real      FITS blank value (1 for each output map)
CH3 omapsz(3)  Integer   Map size (number of integers in each of the 3 maps)
CH3 omaxvl(3)  Real      Maximum counts bin value for the 3 output maps
CH3 ominvl(3)  Real      Minimum counts bin value for the 3 output maps
CH3 oxmaxp(3)  Real      X position of the maximum bin value
CH3 oymaxp(3)  Real      Y position of the maximum bin value
CH3 ozmaxp(3)  Integer   Z index of the maximum bin value
CH3 ocoord     Ch*4      Coordinate system used ('GALA' or 'CELE')
C**************************************************************************
C
C  FTOOL Change
C
C The fits file is no longer read into a buffer.  Rather fitsio is
C is used, so obuffr is no longer used by the code.
C
C  Jeff Silvis
C
C  Sept 1997
C**************************************************************************
CH3 obuffr(3)  Ch*2880   Output FITS buffer for the 3 output maps
CH3 oheadr     Ch*80     Buffer to save the header information of the 2 input
CH3  (2,300)		 maps
CH3 ocntbn     Integer   Counts map bin values
CH3   (360,200,10)
CH3 oexpbn     Real      Exposure map bin values
CH3   (360,200,10)
CH3 ointbn     Real      Intensity map bin values
CH3   (360,200,10)
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  oi         I*4        -      Index of the output map bin on the 1st axis
CH3  oj         I*4        -      Index of the output map bin on the 2nd axis
CH3  ii         I*4        -      Index of the input map bin on the 1st axis
CH3  ij         I*4        -      Index of the input map bin on the 2nd axis
CH3  iil        I*4        -      Leftmost index of input map bin on 1st axis
CH3				  to intersect the output map bin
CH3  iir        I*4        -      Rightmost index of input map bin on 1st axis
CH3				  to intersect the output map bin
CH3  ijb        I*4        -      Bottom index of input map bin on 2nd axis
CH3				  to intersect the output map bin
CH3  ijt        I*4        -      Topmost index of input map bin on 2nd axis
CH3				  to intersect the output map bin
CH3  on1        I*4        -      Number of bins on the output map 1st axis
CH3  inr        I*4        -      Number of bins on the input map 1st axis
CH3  fstext     L*4        -      Determines if this is the 1st time that the 
CH3				  exposure is computed for current output bin
CH3  iflag      L*4        -      Determines if the intensity values for the
CH3				  current bin should be computed at this time
CH3				  (when area is largest and intensity requested)
CH3  tarea      R*8        -      Area of the input bin
CH3  weight     R*8        -      Portion of the input bin area that intersects
CH3				  the output bin
CH3  oxf	R*8	   -      Left x position of the output bin
CH3  oxr	R*8	   -      Right x position of the output bin
CH3  oyb	R*8	   -	  Bottom y position of the output bin
CH3  oyt	R*8	   -	  Top y position of the output bin
CH3  twopi	R*8	   -	  Value of 2 pi in radians
CH3  ibox(2,4)  R*8	   -	  x & y coordinates of 4 points of input bin
CH3  obox(2,4)  R*8	   -	  x & y coordinates of 4 points of output bin
CH3  pt(2)      R*8	   -	  x & y coordinates of a point
CH3  polygn 	R*8	   -	  x & y coordinates of a polygon
CH3   (2,10)
CH3  maxwgh	R*8	   -	  Maximum weight for current the output bin
CH3  ixmax	R*8	   -	  Rightmost x coordinate of input map
CH3  pi		R*8	   -	  Value of pi
CH3
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4
CH4  Method:
CH4    Initialize variables
CH4    For (oj=1 to number of bins on the y axis) do
CH4       If (the output map grid is Aitoff) get number of bins on the x axis
CH4       For (oi=1 to number bins on the x axis) do
CH4	     Compute the coordinates of the 4 points of the output bin
CH4	     For (ol=1 to 4) do
CH4	        Call celgal_a to convert point ol to the other coordinate system
CH4	     End for
CH4	  End for
CH4	  Put the appropriate points in the (0-360) system
CH4	  Find the minimum and maximum points of the bin on the x and y axis
CH4	  Find the min & max indexes of the input bins around the output bin
CH4	  For (ij=min y index to max y index) do
CH4	     If (input map grip type is Aitoff) get number of x bins
CH4	     Find the min & max indexes of the input bins around the output bin
CH4	     Compute the Y coordiantes of the 4 points of the input bin
CH4	     For (ii=min x index to max x index) do
CH4	        Find the coordinates of the 4 points of the input bin
CH4		For (l=1 to 4) do
CH4		   Call inbox to find if point l of input box is in output box
CH4		   Call inbox to find if point l of output box is in input box
CH4		   For (m=1 to 4) do
CH4		      call inters to find if the intersection of output box l
CH4		      and input box m is between the end points
CH4		   End for
CH4		End for
CH4	        If (number of points found above > 2) then
CH4		   Call polyar to get the area of the polygon on intersection
CH4		   Compute the portion of the are of intersection
CH4		   Call addbin_a to add the portion of the input bin to output bin
CH4		End if
CH4	     End for
CH4	  End for
CH4    End for
CH4  End addiff
CH4
CH5 $Log: addiff.f,v $
CH5 Revision 1.7  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.6  1998/09/30 18:22:02  peachey
CH5 Changed addbin to addbin_a and celgal to celgal_a
CH5
c Revision 1.5  1997/11/15  19:37:17  silvis
c Several format statematements were modifided to make the code
c compatible with osf.  Also the max and min statements were changed
c to dmax1 amd dmin1 to make the code compatible with osf.
c
c Jeff Silvis
c
CH5 Revision 1.4  1997/11/03 22:55:47  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:07  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:58:56  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:15:51  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C
C   Revision 2.0    28 April 1997   Jeff Silvis
C   The variable lunout added to the argument call. This variable 
C   provides a logicial number that is used for output to an ascii file.
C   In the original code, this was hardwired to be 3.  Since this is 
C   now an Ftool, the logical number 3 is used by XPI to link with the 
C   parameter file, so the hardwiring causes a conflict.  A value for 
C   lunout was assigned by the fitsio routine ftgiou in the main 
C   routine, eaddamp.  Then lunout is added as an argument to any 
C   routine, such as this one, that has  output to the ascii file.
C
C
C Revision 1.4  1993/05/24  13:47:42  albert
C Corrected problems with bins intersecting the map both on the left and on
C the right boundaries and problems finding the corrcet "inside" of bins.
C
C Revision 1.3  1992/04/16  17:41:38  albert
C Corrected problem with conflicting vaiable name.
C
C Revision 1.2  1992/04/16  16:21:55  albert
C Modified to get the bin data arrays and pass them to the addbin routine so
C that they can be dimensioned variably.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addiff.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine addiff(lunout,in,sign,intflg,icntbn,iexpbn,ocntbn
     & ,oexpbn)
      implicit none 

      integer           lunout
      real              icntbn(*),iexpbn(*),ocntbn(*),oexpbn(*)
      integer   	in,sign,oi,oj,ii,ij,iil,iir,ijb,ijt,ii1
      integer		on1,inl,inr,iret,l,m,n,l1,m1,n3,ol
      logical   	intflg,inters,inbox,fstexp,iflag
       character        id*80
      character 	dir*2
      double precision  tarea,weight,oxf,oxr,oyb,oyt,twopi
      double precision  obox(2,4),ibox(2,4)
      double precision  pt(2),polygn(2,10),polyar,oy1,oy2
      double precision	halfpi,maxx,maxwgh,ixmax,pi,x

      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'
      common /id/id
      id= '$Id: addiff.f,v 1.7 2005/08/26 19:36:34 irby Exp $'

C---> Initialize variables
      pi = atan(1.0) * 4
      twopi = pi*2
      halfpi = pi/2 - 10e-6
      dir = 'CG'
      if (ocoord .eq. 'GALA') dir = 'GC'
      tarea = icdel1*icdel2 
      n3 = min(onaxs3,inaxs3)
      ixmax = icrvl1 + icdel1*inaxs1

C *******************************************************************
C      Ftool Change
C
C OSF does not support the / in a format statemant that is within 
C the actual write statment.  So a separate format statement must be 
C writen. 
C
C      write(lunout,'(/,''Input file'',i2,'' has a different 
C     &               coordinate system than'',
C     &            '' the output file'')') in


  111  format (/,"Input file",i2,
     &        "has a different coordinate system than",
     &        "the output file")
       write(lunout,111) in 

C---> Loop over the Y axis of the output map
      do oj = 1, onaxs2

C------> Find the Y coordinates of the 4 points of the current output bin
	 oy1 = ocrvl2 + ocdel2*(oj-ocrpx2)
	 oy2 = oy1 + ocdel2
	 oy1 = max(oy1, -halfpi)
	 oy2 = min(oy2, halfpi)

C------> Compute the variable X axis parameters in case of Aitoff output map
         on1 = onaxs1
         if (ogridt.eq.'AITF') then
            on1 = onax12(oj)
            ocdel1 = twopi/on1
         end if

C------> Loop over the X axis of the output map
	 do oi = 1, on1

C---------> Find the coordinates of the 4 points of the current output bin
	    obox(1,1) = ocrvl1 + ocdel1*(oi-ocrpx1)
	    obox(1,2) = obox(1,1) + ocdel1
	    obox(1,3) = obox(1,2)
	    obox(1,4) = obox(1,1)
	    obox(2,1) = oy1
	    obox(2,2) = oy1
	    obox(2,3) = oy2
	    obox(2,4) = oy2
	    fstexp = .true.
	    maxwgh = 0.0
	    x = -9999
	    maxx = x
	    inl = 1
	    inr = inaxs1

C---------> Convert the 4 points of the output box to the input map system
	    do ol = 1, 4
	       call celgal_a(dir,obox(1,ol),obox(2,ol),obox(1,ol),
     &                obox(2,ol),iret)
	       if (icrvl1.lt.obox(1,ol).and.obox(1,ol).lt.ixmax)
     &            x = obox(1,ol)
	       if (icrvl1.lt.obox(1,ol)+twopi.and.
     &             obox(1,ol)+twopi.lt.ixmax)
     &		  x = obox(1,ol) + twopi
               if (x.gt.maxx) maxx=x
	    end do

C---------> Find if some of the 4 points should be in the 0-360 system
	    x = maxx
	    if (x .ne. -9999) then
	       do ol = 1, 4
		  if (dabs(x-obox(1,ol)) .gt. dabs(x-obox(1,ol)-twopi))
     &		     obox(1,ol) = obox(1,ol) + twopi
		  if (obox(1,ol) .eq. x) goto 5
		  if (obox(1,ol).gt.ixmax.and.
     &                obox(1,ol)-twopi.gt.icrvl1)
     &		     inr = 99999
		  if (obox(1,ol).lt.icrvl1.and.
     &                obox(1,ol)+twopi.lt.ixmax)
     &		     inl = -99999
5		  continue
	       end do
	    else
	       goto 20
	    end if

C---------> Find the min and max of the output box in the input box system
	    oyb = min(obox(2,1),obox(2,2),obox(2,3),obox(2,4))
	    oyt = max(obox(2,1),obox(2,2),obox(2,3),obox(2,4))
	    oxf = min(obox(1,1),obox(1,2),obox(1,3),obox(1,4))
	    oxr = max(obox(1,1),obox(1,2),obox(1,3),obox(1,4))

C---------> Find the Y indexes of the input bins around the output box
	    ijb = dmax1((oyb - icrvl2) / icdel2 + icrpx2 , 1.0d0)
	    ijt = dmin1((oyt - icrvl2) / icdel2 + icrpx2 , dble(inaxs2))
	    if (ijb.gt.inaxs2 .or. ijt.lt.1) goto 20
  
C---------> Loop over the Y axis of the input bins found above
	    do ij = ijb, ijt

C------------> Compute the variable X axis parameters if Aitoff input map
	       if (igridt.eq.'AITF') then
     	          inr = inax12(ij)
           	  icdel1 = twopi/inr
               end if

C------------> Find the X indexes of the input bins around the output box
	       iil = dmax1((oxf - icrvl1) / icdel1 + icrpx1 , dble(inl))
	       iir = dmin1((oxr - icrvl1) / icdel1 + icrpx1 , dble(inr))
	       if (iil.gt.inr .or. iir.lt.inl) goto 20

C------------> Find the Y coordinates of the 4 points of the input bin
	       ibox(2,1) = icrvl2 + icdel2*(ij-icrpx2)
	       ibox(2,2) = ibox(2,1)
	       ibox(2,3) = ibox(2,1) + icdel2
	       ibox(2,4) = ibox(2,3)

C------------> Loop over the X axis of the input bins found above
	       do ii = iil, iir

C---------------> Find the X coordinates of the 4 points of the input bin
		  ibox(1,1) = icrvl1 + icdel1*(ii-icrpx1)
	          ibox(1,2) = ibox(1,1) + icdel1
	          ibox(1,3) = ibox(1,2)
	          ibox(1,4) = ibox(1,1)

C---------------> Loop to find the points of the polygon of intersection
		  n = 0
		  do l = 1, 4

C------------------> Find if point l of the input box is in the output box
		     if (inbox(.false.,obox,ibox(1,l))) then
		        n = n + 1
			polygn(1,n) = ibox(1,l)
			polygn(2,n) = ibox(2,l)
		     end if

C------------------> Find if point l of the output box is in the input box
		     if (inbox(.true.,ibox,obox(1,l))) then
			n = n + 1
			polygn(1,n) = obox(1,l)
			polygn(2,n) = obox(2,l)
		     end if

C------------------> Find the intersections of all the lines of 2 the boxes
		     do m = 1, 4
			l1 = mod(l,4) + 1
			m1 = mod(m,4) + 1
			if (inters(obox(1,l),obox(1,l1),ibox(1,m),
     &			ibox(1,m1),(mod(m,2).eq.0),pt)) then
			   n = n + 1
			   polygn(1,n) = pt(1)
			   polygn(2,n) = pt(2)
			 end if
		     end do

		  end do

C---------------> If not enough points found in polygon, skip
		  if (n .lt. 3) goto 10

C---------------> Sort polygon, compute its area & the bin weight factor
		  weight = polyar(n,polygn) / tarea
		  if (weight .gt. maxwgh .and. intflg) then
		     maxwgh = weight
		     iflag = .true.
		  else
		     iflag = .false.
		  end if

C---------------> If the bin index is out of the map, adjust it
		  ii1 = ii
		  if (ii.gt.inaxs1) then
		     ii1 = ii - inaxs1
		  else if (ii .lt. 1) then
		     ii1 = inaxs1 + ii
		  end if

C---------------> Add the contribution of the input bin to the output
		  call addbin_a(oi,oj,ii1,ij,n3,iflag,fstexp,weight,
     &                  sign,icntbn,iexpbn,ocntbn,oexpbn)

10	          continue
	       end do

	    end do

20	    continue
	 end do

      end do

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End addiff.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
