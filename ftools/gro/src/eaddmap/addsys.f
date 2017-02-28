CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addsys.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  addsys
CH1
CH1  $Id: addsys.f,v 1.8 2005/08/26 19:36:34 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Adds 2 maps that have the same coordinate system.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  call addsys(in,sign,intflg)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     in          I*4     I   Index of the current input file (1 or 2)
CH2     sign        I*4     I   Sign of the add operation (+1 or -1)
CH2     intflg      L*4     I   Flag to determine if intensity must be generated
CH2
CH2  Called by:  addmap
CH2
CH2  Calls:
CH2   addbin_a: Adds into the current output bin
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
CH3                               to intersect the output map bin
CH3  iir        I*4        -      Rightmost index of input map bin on 1st axis
CH3                               to intersect the output map bin
CH3  ijb        I*4        -      Bottom index of input map bin on 2nd axis
CH3                               to intersect the output map bin
CH3  ijt        I*4        -      Topmost index of input map bin on 2nd axis
CH3                               to intersect the output map bin
CH3  on1        I*4        -      Number of bins on the output map 1st axis
CH3  in1        I*4        -      Number of bins on the input map 1st axis
CH3  fstext     L*4        -      Determines if this is the 1st time that the
CH3                               exposure is computed for current output bin
CH3  iflag      L*4        -      Determines if the intensity values for the
CH3                               current bin should be computed at this time
CH3                               (when area is largest and intensity requested)
CH3  itest	L*4	   -	  Tests if the input map crosses 180 to -180
CH3  otest	L*4	   -	  Tests if the output map crosses 180 to -180
CH3  flip	L*4	   -	  Tests if the input map reference bin must be
CH3				  flipped from 1 coordinate system to another
CH3  tarea      R*8        -      Area of the input bin
CH3  weight     R*8        -      Portion of the input bin area that intersects
CH3                               the output bin
CH3  ixf	R*8	   -	  Leftmost coordinate of input bin
CH3  ixr	R*8	   -	  Rightmost coordinate of input bin
CH3  iyb	R*8	   -	  Bottommost coordinate of input bin
CH3  iyt	R*8	   -	  Topmost coordinate of input bin
CH3  maxwgh     R*8        -      Maximum weight for current the output bin
CH3  oxf        R*8        -      Left x position of the output bin
CH3  oxr        R*8        -      Right x position of the output bin
CH3  oyb        R*8        -      Bottom y position of the output bin
CH3  oyt        R*8        -      Top y position of the output binL
CH3  ixmax      R*8        -      Rightmost x coordinate of input map
CH3  oxmax      R*8        -      Rightmost x coordinate of output map
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4       6              Terminal output
CH4
CH4  Method:
CH4    Initialize variables
CH4    For (oj=1 to number of bins on the y axis) do
CH3	  Find the Y limits of the input map around the output bin
CH4       If (the output map grid is Aitoff) get number of bins on the x axis
CH4       For (oi=1 to number bins on the x axis) do
CH4          Compute the coordinates of the output bin in x
CH4	     For (ij = first y index to last y index) do
CH4	        Compute the width of the intersection in y
CH4		If (the input map is Aitoff) get the number of bins on x
CH4		Compute the coordinates of the inbput bin in x
CH4	        Compute the width of the intersection in x
CH4		Compute the portion of the intersecting area
CH4		Call addbin_a to add the current output bin
CH4	     End for
CH4	  End for
CH4    End for
CH4  End addsys
CH4
CH5 $Log: addsys.f,v $
CH5 Revision 1.8  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.7  2002/12/26 17:05:26  irby
CH5 Split data statements out of declarations for f90 compatibility.
CH5
CH5 Revision 1.6  1998/09/30 18:22:03  peachey
CH5 Changed addbin to addbin_a and celgal to celgal_a
CH5
c Revision 1.5  1997/11/16  21:20:04  silvis
c Several changes were made to make the code compatible with OSF.  Most of these
c changes involved changing the print format statements and modifying the max
c and min functions.
c
c Jeff Silvis
c
CH5 Revision 1.4  1997/11/03 22:55:48  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:09  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:58:58  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:15:52  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C Revision 1.5  1993/05/24  13:52:03  albert
C Allowed tolerance when testing against the value of pi to resolve precision
C problems.
C
C Revision 1.4  1992/06/15  18:02:33  albert
C Corrected problem of crossing (0,360) and (-180,180) boundaries by testing
C every bin coordinates and putting it in the correct range.
C
C Revision 1.3  1992/04/16  17:42:27  albert
C Corrected problem with conflicting variabel name.
C
C Revision 1.2  1992/04/16  16:31:20  albert
C Modified to get the bin data arrays as parameters and pass them to the
C addbin routine so as to make them variable dimension.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addsys.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine addsys(lunout,in,sign,intflg,icntbn,iexpbn,ocntbn
     & ,oexpbn)
      implicit none 

      integer lunout
      real              icntbn(*),iexpbn(*),ocntbn(*),oexpbn(*)
      integer   	in,sign,oi,oj,ii,ij,iil,iir,ijb,ijt,on1,in1,n3
      logical   	intflg,fstexp,iflag
       character        id*80
      double precision  tarea,weight,dx,dy,pi,twopi,ixf,ixr
      double precision  iyb,iyt,maxwgh
      double precision  oxf,oxr,oyb,oyt,ixmax,x,epsilon
      data epsilon/10e-9/

      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'
      common /id/id
      id= '$Id: addsys.f,v 1.8 2005/08/26 19:36:34 irby Exp $'

C---> Initialize variables
      pi = atan(1.0) * 4
      twopi = pi*2
      tarea  = icdel1*icdel2 
      n3 = min(inaxs3,onaxs3)
      write(lunout,*) "Input file",in,
     & "has the same coordinate system as",
     & "the output file"

C---> Find which maps cross the 180 to -180 boundary
      ixmax = icrvl1 + icdel1*inaxs1
      if (ogridt.eq.'AITF') ixmax = icrvl1 + twopi

C---> Loop over the Y axis of the output map
      do oj = 1, onaxs2

C------> Find the Y limits of the input map around the current output bin
	 oyb = ocrvl2 + ocdel2*(oj-ocrpx2)
	 oyt = oyb + ocdel2
	 ijb = dmax1(((oyb - icrvl2) / icdel2) + icrpx2 , dble(1) ) 
	 ijt = dmin1(((oyt - icrvl2) / icdel2) + icrpx2 , dble(inaxs2) ) 
	 if (ijb.gt.inaxs2 .or. ijt.lt.1) goto 20

C------> Compute the variable X axis parameters in case of Aitoff output map
         on1 = onaxs1
         if (ogridt.eq.'AITF') then
            on1 = onax12(oj)
            ocdel1 = twopi/on1
         end if

C------> Loop over the X axis of the output map
	 do oi = 1, on1

	    fstexp = .true.
	    maxwgh = 0.0
	    oxf = ocrvl1 + ocdel1*(oi-ocrpx1)
	    oxr = oxf + ocdel1

C---------> Test in which coordinate system (0,360 or -180,180) the bin is
	    x = -9999
	    if (oxf.gt.pi-epsilon) oxf = oxf - twopi
	    if (oxr.gt.pi+epsilon) oxr = oxr - twopi
	    if (icrvl1.le.oxf.and.oxf.le.ixmax) x = oxf
	    if (icrvl1.le.oxf+twopi.and.oxf+twopi.le.ixmax)
     &         x = oxf + twopi
	    if (icrvl1.le.oxr.and.oxr.le.ixmax) x = oxr
	    if (icrvl1.le.oxr+twopi.and.oxr+twopi.le.ixmax)
     &         x = oxr + twopi
	    if (x .ne. -9999) then 
	       if (dabs(x-oxf) .gt. dabs(x-oxf-twopi)) oxf = oxf + twopi
	       if (dabs(x-oxr) .gt. dabs(x-oxr-twopi)) oxr = oxr + twopi
	    else
	       goto 10
	    end if

C---------> Loop over the Y axis of the input bins around the output bin
	    do ij = ijb, ijt
	       iyb = icrvl2 + icdel2*(ij-icrpx2)
	       iyt = iyb + icdel2
	       dy = min(iyt,oyt) - max(iyb,oyb)

C------------> Compute the variable X axis parameters if Aitoff input map
     	       in1 = inaxs1
	       if (igridt.eq.'AITF') then
     	          in1 = inax12(ij)
                  icdel1 = twopi/in1
      		  tarea  = icdel1*icdel2
               end if

C------------> Find the X limits of the input map for the current output bin
	       iil = dmax1((oxf - icrvl1) / icdel1 + icrpx1 , dble(1) )
	       iir = dmin1((oxr - icrvl1) / icdel1 + icrpx1 , dble(in1))
	       if (iil.gt.in1 .or. iir.lt.1) goto 10

C------------> Loop over the X axis of the input bins around the output bin
	       do ii = iil, iir
		  ixf = icrvl1 + icdel1*(ii-icrpx1)
		  ixr = ixf + icdel1
		  dx = min(ixr,oxr) - max(ixf,oxf)

C---------------> Add the contribution of the current input bin to the output
		  weight = dx*dy / tarea
		  if (weight .gt. maxwgh .and. intflg) then
		     maxwgh = weight
		     iflag = .true.
		  else
		     iflag = .false.
		  end if

		  call addbin_a(oi,oj,ii,ij,n3,iflag,fstexp,weight,sign,
     &                        icntbn,iexpbn,ocntbn,oexpbn)
	       end do

	    end do

10	    continue	      
	  end do

20	 continue
      end do

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End addsys.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
