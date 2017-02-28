CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC chkfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  chkfts
CH1
CH1  $Id: chkfts.f,v 1.4 1997/11/03 22:55:49 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Verifies that the FITS file read conforms to the
CH1            requirements of the addmap program.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  Call chkfts(simple,groups,nlev,ctype1,
CH2                                 ctype2,maxlev,*)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     simple      L*4     I   Determines if FITS is in simple format
CH2     groups      L*4     I   Determines if FITS data is in groups
CH2     nlev        I*4     I   Number of energy levels counted
CH2     ctype1      CH*4    I   Coordinate type on 1st axis
CH2     ctype2      CH*4    I   Coordinate type on 2nd axis
CH2	maxlev	    I*4	    I	Maximum number of energy levels
Ch2	*		    I	Line number to return to in case of error
CH2
CH2  Called by:  readft
CH2
CH2  Calls: None
CH1
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
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  code 	I*4	   0	  Subroutine return code
CH3  sgn        I*4        1      Temporary value of sign (first time: +)
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4
CH4  Method:
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
CH4     If (the coordinate system is not valid) then
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
CH4  End chkfts
CH4
CH5 $Log: chkfts.f,v $
CH5 Revision 1.4  1997/11/03 22:55:49  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:11  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:58:59  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:15:54  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C Revision 1.5  1994/02/04  15:05:48  albert
C Removed the unused maptyp variable since it conflict with local variable
C in readft.
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
C
C Revision 1.4  1994/01/28  20:45:36  albert
C Modified to set the pointing direction of the input map.
C
C Revision 1.3  1993/02/22  14:26:03  albert
C Allowed maps with other coordinate systems (earth centered, instrument
C centered, sun and moon centered) to be added.
C
C Revision 1.2  1992/04/16  16:35:12  albert
C Modified to check for the new input arrays maximum size of maxsiz=648000 and
C for maximum size on the X and Y axis of 720 and 360 respectively.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC chkfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine chkfts(lunout,simple,groups,nlev,ctype1,ctype2,
     &  maxlev,maxsiz,ptdir,*)
      implicit none 

      integer lunout
      real	ptdir(4)
      integer   nlev,code,maxlev,maxsiz
      logical   simple,groups
      character ctype1*4,ctype2*4 
       character        id*80

      include 'infits.cmn.inc'
      common /id/id

      id= '$Id: chkfts.f,v 1.4 1997/11/03 22:55:49 silvis Exp $'
      code = 0

C---> Test for valid number of bits per pixel
      if (ibitpx(1).ne.16.and.ibitpx(1).ne.32) then
         write(lunout,*) 
     &    'chkfts: Invalid number of bits per pixels',ibitpx
         code = 1
      endif

C---> Test for correct axis values
      if (inaxis.ne.3.or.inaxs1.lt.3.or.inaxs2.lt.3) then
         write(lunout,*) 
     &    'chkfts: Bad naxis,naxis1 or 2',inaxis,inaxs1,inaxs2
         code = 1
      endif
      if (inaxs1.gt.ixl.or.inaxs2.gt.iyl) then
         write(lunout,*) 'chkfts: Bad naxis1 or naxis2',inaxs1,inaxs2
         write(lunout,*) '        Must be <= to',ixl,iyl
         code = 1
      endif
      if (inaxs1*inaxs2*inaxs3.gt.maxsiz) then
         write(lunout,*) 'Error: too much data in the input map:'
         write(lunout,*) '       naxis1*naxis2*naxis3 is >',maxsiz
         return 1
      endif
      if (inaxs3.lt.1.or.inaxs3.gt.maxlev.or.inaxs3.ne.nlev) then
         write(lunout,*) 'chkfts: Bad naxis3 / # E levels',inaxs3,nlev
         code = 1
      endif

C---> Test that the data type is event counts
      if (ibunit(1).ne.'COUNTS') then
         write(lunout,*) 'chkfts: Bad data type ',ibunit(1)
         code = 1
      endif

C---> Test the coordinate system
      if (ctype1.eq.'GLON'.and.ctype2.eq.'GLAT') then
         icoord = 'GALA'
      else if (ctype1.eq.'RA'.and.ctype2.eq.'DEC') then
         icoord = 'CELE'
	 ptdir(1) = ptdir(3)
	 ptdir(2) = ptdir(4)
      else if (ctype1.eq.'LATD'.or.ctype2.eq.'LONG') then
         icoord = 'ERTH'
      else if (ctype1.eq.'TETX'.or.ctype2.eq.'TETY') then
         icoord = 'INST'
      else if (ctype1.eq.'GTLO'.or.ctype2.eq.'GTLA') then
         icoord = 'TGAL'
      else if (ctype1.eq.'CTRA'.or.ctype2.eq.'CTDE') then
         icoord = 'TCEL'
      else if (ctype1.eq.'SLON'.or.ctype2.eq.'SLAT') then
         icoord = 'SUN'
      else if (ctype1.eq.'MLON'.or.ctype2.eq.'MLAT') then
         icoord = 'MOON'
      else
         write(lunout,*) 
     &     'chkfts: bad coordinate system ',ctype1,',',ctype2
         code = 1
      endif

C---> Test the grid type
      if (igridt.eq.'RECT') then
         if (.not.simple.or.groups) then
            write(lunout,*)
     &       'chkfts: rectangular grid but non-standard format'
            code = 1
         endif
      else if (igridt.eq.'AITF') then
         if (simple.or..not.groups.or.iprcnt.lt.1.or.igrcnt.lt.1) then
            write(lunout,*) 'chkfts: bad grid/simple/group combination'
            code = 1
         endif
      else
         write(lunout,*) 'chkfts: bad grid type',igridt
         if (igridt.eq.'POLA') write(lunout,*) 
     &     'Polar maps are not permitted'
         code = 1
      end if

      return code

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End chkfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
