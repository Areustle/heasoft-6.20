CCCCCCCCCCCCCCCCCCCCCCCCCreadin.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  readin
CH1
CH1  $Id: readin.f,v 1.6 2005/08/26 19:36:34 irby Exp $
CH1
CH1   Ftool conversion completed on 8 Aug 1997.
CH1   Primary changes:
CH1     1. Input of user data through ascii file replaced by Xanadu Parameter Interface (XPI)
CH1     2. Writing to ascii error file done through the logical number lunout not the fixed 
CH1        number 3.
CH1     3. Status variable added to track fitsio errors.
CH1
CH1   Jeff Silvis HSTX 
CH1
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Read the user input and initialize program
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  call readin(filnam,sign,intflg,*)
CH2   subroutine readin(lunout,filnam,sign,intflg,maxsiz,ocntbn,oexpbn,ointbn,
CH2     &  status,*)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	filnam(3)   Ch*80   O	Counts file names (1st input, 2nd input, output)
CH2	sign	    I*4	    O	Sign of the map operation (+1 or -1)
CH2	intflg	    L*4     O	User requested the intensity map flag
CH2     maxsiz      I*4         Max number of elements for ocntbn, oexpbn & ointbn
CH2     ocntbn      I*4         Counts map bin values    (360,200,10)
CH2     oexpbn      R*4         Exposure map bin values  (360,200,10)
CH2     ointbn      R*4         Intensity map bin values (360,200,10)
C********************************************************************************
CH2    Ftool change  New variables added 
CH2
CH2    lunout        I*4         gives a logical number for the asscii output file
CH2                               (see below)  
CH2    status         I*4         gives the error status of the fitsio functions
CH2
CH2    J. Silvis HSTX 
CH2     6 Aug 1997  
CH2
CH2
C**********************************************************************************
C
CH2	*	   		Where to return to in case of error
CH2
CH2  Called by:  eaddmap
CH2
CH2  Calls:
CH2   uclgst - Gets a character variable from the parameter file using XPI
CH2   uclgsi - Gets an integer from the parameter file using XPI
CH2   uclgsb - Gets a boolean variable from the parameter file using XPI
CH2   uclgsd - Gets a double precision variable from the parameter file using XPI
CH2
CH3  COMMON Use:
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
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  sign       I*4        -      Sign in the 2 maps operation (+1 or -1)
CH3  sgn        I*4        -      Temporary value of the sign
CH3  intns      L*4        -      Temporary value of the intensity flag
C**************************************************************************************
C     FTOOL Change
C
C     The variable newfil has been dropped from the ftool.  It told the 
C     user whether or not a file by the same name already exists on disk.
C     I believe that this information was passed from a higher level GUI 
C     that no longer exits.  This feature is of minimal value, so I decided 
C     drop it.
C
C       J Silvis   29 April 1997
C
C**************************************************************************************
CH3  newfil     L*4	   -	  Output file is new flag
CH3  fname(3)   Ch*80      -      Temporary values of the file names
CH3  maptyp     Ch*16      -      Map type (sky region, galactic disk, all sky)
CH3  xbinsz     R*8        -      Bin size on the x axis
CH3  ybinsz     R*8        -      Bin size on the y axis
CH3  binsz	R*8	   -	  X bin size in degrees
CH3  skylim(4)  R*8	   -	  Sky region coordinates
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing, addmap.list
CH4       5              Namelist file, namelist.add.
CH4
CH4  Method:
CH4    Read the user input from the parameter file using XPI
CH4    Write the user input to the list file
CH4    Compute the FITS parameters of the output map
CH4    If (the output map is rectangular) then
CH4	  Set the parameters for the rectangular map
CH4	  Test for a valid number of bins per axis
CH4    Else
CH4	  Set the parameters for the Aitoff map
CH4	  Test for a valid number of bins per axis
CH4    End if
CH4    Initialize variables for the output map
CH4  End readin
CH4
CH5 $Log: readin.f,v $
CH5 Revision 1.6  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.5  2001/11/23 19:42:12  irby
CH5 Fix uninitialized variables to prevent warnings.
CH5
CH5 Revision 1.4  1997/11/03 22:55:54  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:22  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:59:06  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:16:05  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C************************************************************************************
C   FTOOL Change
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
C*************************************************************************************
C Revision 1.4  1994/01/28  20:49:03  albert
C Modified to print the value of the cutoff angle
C
C Revision 1.3  1993/02/22  14:33:39  albert
C Allowed more coordinate systems to be used (earth, instrument, sun or moon
C centered).
C
C Revision 1.2  1992/04/16  16:51:20  albert
C Modified to use variable dimension bin data arrays and to use new limits
C on the number of bins on the X and Y axis.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC readin.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine readin(lunout,filnam,sign,intflg,maxsiz,ocntbn,
     &  oexpbn,ointbn,status,*)
      implicit none
      integer           lunout 
C************************************************************************************
C      FTOOL Change
C
C     The variable sgn was used for input from the ascii file.
C     Since the input is now from XPI we can drop it.
C
C     J. Silvis   30 April 1997
C
C      integer   	maxsiz,sign,sgn,i,j,n,nphi,g1,g2
C***********************************************************************************
      integer   	maxsiz,sign,i,j,n,nphi,g1,g2,ncutoff
      real		ocntbn(maxsiz),oexpbn(maxsiz),ointbn(maxsiz)
C**********************************************************************************
C      FTOOL Change
C
C      Newfil has been dropped (see note above)
C        J. Silvis 29 April 1997
C
C      logical   	intflg,intns,newfil,testpi
C
C
C
C
C      FTOOL Change
C
C     The variables intns and fname were used for input from the ascii file.
C     Since the input is now from XPI we can drop them.
C
C     J. Silvis   30 April 1997
C
C*************************************************************************************
      logical   	intflg,testpi
C      character 	filnam(3)*256,fname(3)*80,id*80,maptyp*16
      character 	filnam(3)*256,maptyp*16,ncoord*32
       character        id*80
      double precision  xbinsz,ybinsz,binsz,skylim(4),conv,twopi
      double precision  sarea,daref,theta1,theta2
      integer status
      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'
C
C
C
C
C
C
C        
C      FTOOL Change
C
C     All input is now done through XPI.
C      
C        J. Silvis 30 April 1997
C
C      namelist /userin/fname,sgn,intns,newfil,ocoord,xbinsz,ybinsz,skylim,
C     &          maptyp,cutoff
C
C

      common /id/id
      id= '$Id: readin.f,v 1.6 2005/08/26 19:36:34 irby Exp $'

      daref = 0.0
      twopi = atan(1.0) * 8
      conv = twopi/360.0
C***************************************************************************
C
C            Ftool Change
C
C  All fitsio routines use "inherited status" convention.  This means that 
C  if an error has already occurred (i.e. status has a value other than 0)
C  then a subroutine will exit immediately without changing the value of
C  status.   This convention will be maintained for the EGRET I/O.  (See 
C  section 4.6 of the FITSIO User's Guide Version 5.0 by William Pence)
C
       if (status .ne. 0 )  then 
           return
       endif
C
C    J. Silvis HSTX 
C     6 Aug 1997  
C
CC***************************************************************************  
C
C   FTOOL Change
C   
C   The input is done through XPI, so the read(5,userin) is replaced 
C   with calls to the uclgs routines
C 
C   J. Silvis   30 April 1997
C
C---> Read and write the user input
C      open(5, file='namelist.add')
C      read(5,userin)
C     Have the input come from an ftool parameter file instead of an 
C     ascii file (the old namelist.add)
C
C      filnam(1) = fname(1)
C      filnam(2) = fname(2)
C      filnam(3) = fname(3)       
C      sign = sgn
C      intflg = intns 
C
C---> Input from XPI
C******************************************************************************
       call uclgst('infile1',filnam(1) , status)
       call uclgst('infile2',filnam(2) , status)
       call uclgst('outfile',filnam(3), status)
       call uclgsi('sign', sign, status)
       call uclgsb('intflg', intflg , status)
       call uclgst('ocoord' ,ncoord, status)       
       ocoord = ncoord(1:4)
       call uclgst('maptyp', maptyp, status)       
       call uclgsd('xbinsz',xbinsz, status)       
       call uclgsd('ybinsz',ybinsz, status) 
       call uclgsi('cutoff', ncutoff, status)
       cutoff = ncutoff
       call uclgsd('lonmin',skylim(1), status) 
       call uclgsd('lonmax',skylim(2), status) 
       call uclgsd('latmin',skylim(3), status) 
       call uclgsd('latmax',skylim(4), status) 

C---> Write the user input
      write(lunout,'(///,''USER INPUT:'',/,''-----------'')')
      write(lunout,*) 'Name of the 1st input file :',filnam(1)
      write(lunout,*) ' '
      write(lunout,*) 'Name of the 2nd input file :',filnam(2)
      write(lunout,*) ' '
      write(lunout,*) 'Name of the output file    :',filnam(3)
      write(lunout,*) ' '
C*****************************************************************
C      FTOOL Change
C
C      Newfil has been dropped (see note above)
C        J. Silvis 29 April 1997
C
C      if (newfil) wriskylim(1)te(lunout,*) 'The output file is new'
C      if(.not.newfil)write(lunout,*) 'The output file will replace the 1st input file'
C
C
C*******************************************************************

      if (sign.eq.1)  write(lunout,*) 
     &       'The input files will be added'
      if (sign.eq.-1) 
     & write(lunout,*) 
     &       'The second input file will be subtracted from the first'
      if (intflg) write(lunout,*) 
     &       'The corresponding Exposure & Intensity maps ',
     &   'will be created with the Counts map'
      if (.not.intflg) write(lunout,*) 
     &       'Only the Counts map will be generated'

C---> Write information about the output map
      write(lunout,'(/,''Output Map Parameters:'')')
      write(lunout,*) 'Map Type ........................ = ',maptyp
      if (maptyp .eq. 'All Sky') then
	 ogridt = 'AITF'
	 skylim(1) = -180.0
	 skylim(2) = +180.0
	 skylim(3) = -90.0
	 skylim(4) = +90.0
         write(lunout,*) 
     &       'Grid Style .................. = Aitoff'
      else if (maptyp .eq. 'Galactic Disk') then
	 ogridt = 'RECT'
	 skylim(1) = -180.0
	 skylim(2) = +180.0
	 skylim(3) = -20.0
	 skylim(4) = +20.0
         write(lunout,*) 
     &       'Grid Style .................. = Rectangular'
      else
	 ogridt = 'RECT'
         write(lunout,*) 
     &       'Grid Style .............. = Rectangular'
      end if
      if (ocoord .eq. 'GALA') then
         write(lunout,*) 
     &       'Coordinate System ........... = Galactic'
      else if (ocoord .eq. 'CELE') then
         write(lunout,*) 
     &       'Coordinate System ........... = Celestial'
      else if (ocoord .eq. 'ERTH') then
         write(lunout,*) 
     &       'Coordinate System ........... = Earth Centered'
      else if (ocoord .eq. 'INST') then
         write(lunout,*) 
     &       'Coordinate System ........... = Instrument Centered'
      else if (ocoord .eq. 'TGAL') then
         write(lunout,*) 
     &       'Coordinate System ........... = Transformed Galactic'
      else if (ocoord .eq. 'TCEL') then
         write(lunout,*) 
     &       'Coordinate System ........... = Transformed Celestial'
      else if (ocoord .eq. 'SUN') then
         write(lunout,*) 
     &       'Coordinate System ........... = Sun Centered'
      else if (ocoord .eq. 'MOON') then
         write(lunout,*) 
     &       'Coordinate System ........... = Moon Centered'
      end if

C---> Write the bin size selected by the user
      write(lunout,'(''Horizonthal/Longitude Bin Size .. ='',f9.2)') 
     &       xbinsz
      write(lunout,'(''Vertical/Latitude Bin Size ...... ='',f9.2)') 
     &       ybinsz
 
C---> Write the region of the sky selected
      if (skylim(1).gt.180.0) skylim(1) = skylim(1) - 360.0
      if (skylim(2).gt.180.0) skylim(2) = skylim(2) - 360.0
      testpi = .false.
      if (skylim(1).ge.skylim(2)) testpi = .true.
      write(lunout,'(''Horizonthal/Longitude Coordinates ='',2(f9.2))')
     &   skylim(1),skylim(2)
      write(lunout,'(''Vertical/Latitude Coordinates ... ='',2(f9.2))')
     &   skylim(3),skylim(4)
      if (skylim(3).eq.skylim(4)) then
         write(lunout,'(/,'' ERROR: The map boundaries are invalid'')')
         goto 1000
      end if
      if (cutoff .ne. 0) 
     &write(lunout,'(''Cutoff Angle .................... ='',i9)')cutoff

C---> Compute the FITS parameters of the output map
      obitpx = 16
      onaxis = 3
      obscal(1) = 1.0
      obzero(1) = 0.0
      ocrvl1 = skylim(1) * conv
      ocrpx1 = 1
      ocdel1 = xbinsz * conv
      ocrvl2 = skylim(3) * conv
      ocrpx2 = 1
      ocdel2 = ybinsz * conv
      runnum = 0
      oindex(1) = 0
      oindex(2) = 0

C---> Set the output file parameters for the rectangular maps
      if (ogridt.eq. 'RECT') then
         if (testpi) then
	    onaxs1 = nint((360.0+skylim(2)-skylim(1)) / xbinsz)
         else
	    onaxs1 = nint((skylim(2)-skylim(1)) / xbinsz)
         endif
         onaxs2 = nint((skylim(4)-skylim(3)) / ybinsz)

C------> Test that the number of bins per axis is valid
         if (mod(onaxs1,2).ne.0) then
	    onaxs1 = onaxs1 - 1
	    skylim(2) = skylim(2) - xbinsz
	    write(lunout,*)'Warning: uneven # of bins. naxis1 set to',
     &                     onaxs1
         endif
         if (mod(onaxs2,2).ne.0) then
	    onaxs2 = onaxs2 - 1
	    skylim(4) = skylim(4) - ybinsz
	    write(lunout,*)'Warning: uneven #bins. naxis2 set to',onaxs2
         endif
         omapsz = onaxs1*onaxs2

C---> Set the output file parameters for the Aitoff maps
      else if (ogridt .eq. 'AITF') then
	 onaxs1 = nint(twopi / ocdel1)
	 onaxs2 = nint(twopi / (2.0*ocdel2))
	 ogrcnt = onaxs2
	 oprcnt = 5
	 omapsz = 0
	 binsz  = xbinsz * conv

C------> Initialize loop variables
	 n = onaxs2/2
	 theta1 = 0
	 sarea = 0

C------> Loop over the # of latitudes/2. compute the # of bins
	 do i=1,n
	    theta2 = theta1 + binsz
	    sarea = twopi * (sin(theta2) - sin(theta1))
	    if (i.eq.1) then
	       nphi = twopi / binsz
	       daref = sarea / nphi
	    else
	       nphi = sarea / daref + 0.5
	    endif

C---------> Compute the parameters for the southern hemisphere
	    g1 = n + i
	    oftprm(1,g1) = nphi
	    oftprm(2,g1) = -180.0
	    oftprm(3,g1) = theta1 / conv
	    oftprm(4,g1) = 360.0 / nphi
	    oftprm(5,g1) = xbinsz
	    onax12(g1) = nphi

C---------> Compute the parameters for the northern hemisphere
	    g2 = n - i + 1
	    onax12(g2) = nphi
	    do j=1,oprcnt
	       oftprm(j,g2) = oftprm(j,g1)
	    end do
	    oftprm(3,g2) = -oftprm(3,g1)

	    theta1 = theta2
	    omapsz = omapsz + 2 * (oprcnt + oftprm(1,g1))
	 end do

      endif

C---> Test that the number of bins per axis is valid
      if (onaxs1.lt.10.or.onaxs1.gt.oxl) then
	 write(lunout,*) 'Error: bad # of bin for naxis1:',onaxs1
	 write(lunout,*) '       the axis limits are: 10 to',oxl
	 goto 1000
      endif
      if (onaxs2.lt.10.or.onaxs2.gt.oyl) then
	 write(lunout,*) 'Error: bad # of bin for naxis2:',onaxs2
	 write(lunout,*) '       the axis limits are: 10 to',oyl
	 goto 1000
      endif

C---> Initialize variables for the output map
      do i=1,3
	 omaxvl(i) = -1.0e30
	 ominvl(i) = +1.0e30
      end do
      do i=1,maxsiz
         ocntbn(i) = 0
         oexpbn(i) = 0
         ointbn(i) = 0
      end do

      return

1000  continue
      return 1

CCCCCCCCCCCCCCCCCCCCCCC End readin.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end


