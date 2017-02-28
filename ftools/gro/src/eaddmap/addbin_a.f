CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addbin_a.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  addbin_a
CH1
CH1  $Id: addbin_a.f,v 1.4 1998/09/30 18:22:02 peachey Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Add data into the current bin of the output maps.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  call addbin_a(oi,oj,ii,ij,elevel,iflag,fstexp,weight,sign)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     oi	    I*4     I   Index of the output bin on the first axis
CH2	oj	    I*4	    I   Index of the output bin on the second axis
CH2     ii	    I*4     I   Index of the input bin on the first axis
CH2	ij	    I*4	    I   Index of the input bin on the second axis
CH2	elevel	    I*4     I   Number of energy levels in the map
CH2	iflag	    L*4     I   Flag to compute the intensity and exposure bins
CH2	fstexp	    L*4    I/O  Flag to check if this is the first time for the
CH@				output bin that the exposure is calculated
CH2	weight	    R*8	    I   Weight factor when adding into the output bin
CH2	sign	    I*4	    I	Sign of the add operation (+1 or -1)
CH2
CH2  Called by:  addiff, addsam, addsys
CH2
CH2  Calls: None
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
CH3  exp(10)    R*4        -      Original value of the exposure for the current
CH3                               output bin for each energy level
CH3  k          I*4        -      Energy level index
CH3  osoln      R*8        -      Solid angle for the output bin
CH3  isoln      R*8        -      Solid angle for the input bin
CH3
CH4  Logical Units Used: None
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    For (k=1 to number of energy levels) do
CH4       Add (or subtract) weighted input bin to current output bin
CH4    End for
CH4    if (the intensity map is requested) then
CH4       For (k=1 to number of energy levels) do
CH4          If (1st time the exposure for current bin is computed) then
CH4	        Save the original value of the exposure bin
CH4	     end if
CH4	     Compute the output bin solid angle
CH4	     Compute the input bin solid angle
CH4	     Add (or subtract) the weighted exposure to the original value
CH4	  end for
CH4	  Set the flag for 1st on the exposure to false
CH4    end if
CH4  End addbin_a
CH4
CH5 $Log: addbin_a.f,v $
CH5 Revision 1.4  1998/09/30 18:22:02  peachey
CH5 Changed addbin to addbin_a and celgal to celgal_a
CH5
c Revision 1.3  1997/11/03  22:55:47  silvis
c The files used to define the global variables were called: oufits.cmn.f
c and infits.cmn.f and the make file was trying to compile them.  Their
c names were changed to oufits.cmn.inc and infits.cmn.inc and
c were changed in the programs that used these files.
c
CH5 Revision 1.2  1997/09/18 19:38:07  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:15:47  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
c Revision 1.3  1993/05/24  13:43:41  albert
c Changed input counts map array from integer to real to preserve precision
c in low counts maps.
c
c Revision 1.2  1992/04/16  16:19:47  albert
c Modified to pass the bin data arrays to the routine so that they can be
c dimensioned variably.
c
c Revision 1.1  1992/01/29  19:54:11  albert
c Initial revision
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addbin_a.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine addbin_a(oi,oj,ii,ij,elevel,iflag,fstexp,weight,sign,
     &                  icntbn,iexpbn,ocntbn,oexpbn)
      implicit none 

      real      	exp(10)
      integer   	oi,oj,ii,ij,elevel,k,sign
      logical		iflag,fstexp
       character        id*80
      double precision  weight,osoln,isoln

      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'

      real    icntbn(inaxs1,inaxs2,inaxs3)
      real    iexpbn(inaxs1,inaxs2,inaxs3)
      real    ocntbn(onaxs1,onaxs2,onaxs3),oexpbn(onaxs1,onaxs2,onaxs3)

      common /id/id
      id= '$Id: addbin_a.f,v 1.4 1998/09/30 18:22:02 peachey Exp $'

C---> Add the counts bins for all energy levels
      do k=1, elevel
         ocntbn(oi,oj,k) = ocntbn(oi,oj,k) + sign*weight*icntbn(ii,ij,k)
      end do

C---> Add the exposure bins if requested
      if (iflag) then
	 do k=1, elevel
	    if (fstexp) exp(k) = oexpbn(oi,oj,k)
	    osoln = ocdel1*ocdel2 * cos(ocrvl2+ocdel2*(oj-ocrpx2+0.5))
	    isoln = icdel1*icdel2 * cos(icrvl2+icdel2*(ij-icrpx2+0.5))
     	    oexpbn(oi,oj,k) = exp(k) + sign*iexpbn(ii,ij,k)*osoln/isoln
	 end do
	 fstexp = .false.
      end if

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End addbin_a.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
