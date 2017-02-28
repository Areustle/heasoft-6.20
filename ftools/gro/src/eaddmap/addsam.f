CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addsam.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  addsam
CH1
CH1  $Id: addsam.f,v 1.5 1998/09/30 18:22:03 peachey Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Main program to add 2 sky maps in FITS format.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence: call addsam(in,sign,intflg)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	in	    I*4	    I 	Index of the current input file (1 or 2)
CH2	sign	    I*4	    I	Sign of the add operation (+1 or -1)
CH2	intflg	    L*4	    I	Flag to determine if intensity must be generated
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
CH3  sign       I*4        -      Sign in the 2 maps operation (+ or -)
CH3  sgn        I*4        1      Temporary value of sign (first time: +)
CH3  intflg     L*4        -      Determines if intensity map is to be created
CH3  today      Ch*24      -      Current date
CH3  case       Ch*4       -      Case for adding 2 maps
CH3  filnam(3)  Ch*80      -      Names of the maps (1st input, 2nd input, out)
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4       6              Terminal output
CH4
CH4  Method:
CH4    Initialize variables
CH4    For (j=1 to number of bins on y axis) do
CH4       If (Aitoff map) get the number of bins on the x axis
CH4       For (i=1 to the number of bins on the x axis) do
CH4	     Call addbin_a to add the input bin to the output bin
CH4	  End for
CH4    End for
CH4  End addsam
CH4
CH5 $Log: addsam.f,v $
CH5 Revision 1.5  1998/09/30 18:22:03  peachey
CH5 Changed addbin to addbin_a and celgal to celgal_a
CH5
c Revision 1.4  1997/11/03  22:55:48  silvis
c The files used to define the global variables were called: oufits.cmn.f
c and infits.cmn.f and the make file was trying to compile them.  Their
c names were changed to oufits.cmn.inc and infits.cmn.inc and
c were changed in the programs that used these files.
c
CH5 Revision 1.3  1997/09/18 19:38:08  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:58:57  silvis
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
C Revision 1.2  1992/04/16  16:29:23  albert
C Modified to get the bin data arrays as parameters and pass them to the
C addbin routine so as to make them variable dimension.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC addsam.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine addsam(lunout,in,sign,intflg,icntbn,iexpbn,ocntbn,
     & oexpbn)
      implicit none 

      integer           lunout
      real		icntbn(*),iexpbn(*),ocntbn(*),oexpbn(*)
      integer   	in,sign,naxis1,naxis2,naxis3,i,j,n1
      logical   	intflg,fstexp
       character        id*80
      double precision  twopi
  
      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'

      common /id/id
      id= '$Id: addsam.f,v 1.5 1998/09/30 18:22:03 peachey Exp $'

C---> Initialize variables
      twopi = atan(1.0) * 8
      naxis3 = min(inaxs3,onaxs3)
      naxis2 = min(inaxs2,onaxs2)
      naxis1 = min(inaxs1,onaxs1)
      write(lunout,*) "Input file",in,  
     &   "has the same parameters as the output file"

C---> Loop over all the bins on the Y axis
      do j=1,naxis2

C------> Loop over the bins on the X axis (compute the Aitoff number of bins)
         n1 = naxis1
         if (ogridt.eq.'AITF') then
            n1 = onax12(j)
            ocdel1 = twopi/n1
            icdel1 = ocdel1
         end if

C------> Add the current bin
	 do i=1,n1
	    fstexp = .true.
	    call addbin_a(i,j,i,j,naxis3,intflg,fstexp,1.0d0,sign,
     &			icntbn,iexpbn,ocntbn,oexpbn)
	 end do

      end do

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End addsam.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
