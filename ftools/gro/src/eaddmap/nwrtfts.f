CH1  Routine Name:  nwrtfts
CH1
CH1  This program is a complete rewrite of the EGRET routine wrtfts.
CH1  The header writing components were put in the routine wrthd.  While
CH1  this routine writes the data and calls wrthd.  The original wrtfts 
CH1  is included in the comments below.  This function currently cannot 
CH1  process Aitoff maps.
CH1  
CH1  Jeff Silvis HSTX 1 Aug 1997
CH1
CH1  Function: Writes the counts map and if requested, the exposure map
CH1	       and intensity map are generated if intflg is set to 1.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1
CH2  Calling Sequence: 
CH2  Call nwrtfts(cfilnm,sign,intflg,ocntbn,oexpbn,ointbn,status,*)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	cfilnm	    Ch*80   I	Output counts file name
CH2	sign	    I*4	    I   Sign for the map operation (+1 or -1)
CH2	intflg	    I*4     I   Flag to generate the intensity & exposure maps
CH2     ocntbn      I*4         Counts map bin values (360,200,10)
CH2     oexpbn      R*4         Exposure map bin values (360,200,10)
CH2     ointbn      R*4         Intensity map bin values (360,200,10)
CH2     status      I*4         gives the error status of the fitsio functions
CH2	*			Where to return to in case of error
CH2
CH2  Called by:  eaddmap
CH2
CH2  Calls:
CH2**************************************************************************
CH2   Routines used by wrtfts.f but NOT by nwrtfts.f
CH2
CH2   fopen : System routine to open a file
CH2   fread : System routine to read a record
CH2   fwrite: System routine to write a record
CH2   wrtlin: Writes a header line
CH2   fclose: System routine to close a file
CH2**************************************************************************
CH2 
CH2    Routines actually USED by nwrtfts.f  
CH2

CH2   deletefile: checks to see if fits file of a given name exists, if 
CH2               so it deletes the file to avoid conflict when fitsio
CH2               trys to create a file with the same name
CH2   ftgiou:     allocates a logical number that does not conflict with fitsio
CH2   ftinit:     opens and initializes a new FITS file
CH2   wrthd:      writes the header information      
CH2   ftp3de:     writes a three dimensional "cube" of data into a fits file
CH2   ftfiou:     frees a logical number
CH2   ftclos:     closes a fits file
CH2
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
CH3 onaxs1     Integer   Number of bins on the first axis
CH3 onaxs2     Integer   Number of bins on the second axis
CH3 onaxs3     Integer   Number of bins on the third axis (energy levels)
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  efilnm	Ch*80      -	  Output exposure file name
CH3  ifilnm	Ch*80      -	  Output intensity file name
CH3
CH4
CH4  Method:
CH4  
CH4  Create a name for the intensity and exposure files if intflg is 1
CH4  Delete fits files that have the same names as the files we plan to write
CH4  Allocate logical numbers and initialize fits file for input
CH4  Iniatialize global variables that will be written into the header
CH4  Call wrthd.f to write the headers
CH4  Call ftp3de to write the data into the fits file
CH4  Close the fits files and free the logical number.
CH4
C
C
C
C
C
C
C
C
C
C
C
C
C   Below is a copy of the old EGRET program wrtfts that this code is based on
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC wrtfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  wrtfts
CH1
CH1  $Id: nwrtfts.f,v 1.3 1997/11/03 22:55:53 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Writes the counts map and if requested, the exposure map
CH1	       generated
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  Call wrtfts(cfilnm,sign,intflg,*)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	cfilnm	    Ch*80   I	Output counts file name
CH2	sign	    I*4	    I   Sign for the map operation (+1 or -1)
CH2	intflg	    I*4     I   Flag to generate the intensity & exposure maps
CH2	*			Where to return to in case of error
CH2
CH2  Called by:  addmap
CH2
CH2  Calls:
CH2   fopen : System routine to open a file
CH2   fread : System routine to read a record
CH2   fwrite: System routine to write a record
CH2   wrtlin: Writes a header line
CH2   fclose: System routine to close a file
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
CH3  totcnt     I*4        -      Total number of counts
CH3  ingbin     R*4        -      Intensity bins data for galactic disk
CH3  cntbuf     I*2        -      Counts map output buffer
CH3  (1440)
CH3  expbuf     I*2        -      Exposure map output buffer
CH3  (1440)
CH3  intbuf     I*2        -      Intensity map output buffer
CH3  (1440)
CH3  efilnm	Ch*80      -	  Output exposure file name
CH3  ifilnm	Ch*80      -	  Output intensity file name
CH3  coment     Ch*47      -      Comment for FITS header line
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       2              Output counts file
CH4       3              Program output listing
CH4      20              Output exposure file
CH4      30              Output intensity file
CH4
CH4  Method:
CH4     Set the FITS keywords for the coordinate system
CH4     Open the output FITS files
CH4     If (the map is rectangular) then
CH4	   Call wrtlin to write the standard FITS header lines
CH4     Else
CH4	   Call wrtlin to write the standard FITS header lines
CH4     End if
CH4     If (intensity file requested) then
CH4	   Call wrtlin to write keywords specific to intensity & exposure maps
CH4	End if
CH4     Call wrtlin to write the lines saved from the input files
CH4     Call wrtlin to write the energy levels
CH4     Call wrtlin to write the file names
CH4     Find the end of the output header
CH4     For (k=1 to the number of energy levels) do
CH4        If (the grid is rectangular) then
CH4           For (j=1 to the number of bins on axis 2) do
CH4              For (i=1 to the number of bins on axis 1) do
CH4                 Unscale the exposure bin and store in output buffer
CH4                 Unscale the intensity bin and store in output buffer
CH4                 If (the output buffer is full) then
CH4                    Write the exposure record to the FITS file
CH4                    Write the intensity record to the FITS file
CH4                 Endif
CH4              End for
CH4           End for
CH4        Else if (the grid is Aitoff) then
CH4           For (j=1 to the number of bins on axis 2) do
CH4              Store each parameter in the output record
CH4              If (the output records are full) write the records
CH4              For (i=1 to the number of bins on axis 1) do
CH4                 Unscale and store the exposure bin data in record
CH4                 Unscale and store the intensity bin data in record
CH4                 If (the output record is full) then
CH4                    Write the exposure record to the FITS file
CH4                    Write the intensity record to the FITS file
CH4                 Endif
CH4              End for
CH4           End for
CH4        End if
CH4        If (the output record is non-empty) then write the record
CH4	End for
CH4  End wrtfts
CH4
CH5 $Log: nwrtfts.f,v $
CH5 Revision 1.3  1997/11/03 22:55:53  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.2  1997/09/18 19:38:20  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:16:02  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
c Revision 1.9  1995/01/04  17:45:08  albert
c Modified to allow up to 999 viewing periods to be added instead of only 99
c
c Revision 1.8  1994/12/14  14:14:13  albert
c Modified to search for string 'counts' instead of just '.' when trying to
c generate the exposure file name from the counts file name.
c
c Revision 1.7  1993/05/24  14:06:56  albert
c Wrote the output counts as real instead of integer.
c
c Revision 1.6  1993/05/05  13:08:42  albert
c Modified to write the output map with the bin center convention
c
c Revision 1.5  1993/02/22  14:38:29  albert
c Modified to allow more coordinate systems to be used and to copy saved
c header entries from input file 1 to the output file header. File names and
c run number of input files also copied in output file header.
c
c Revision 1.4  1993/01/29  15:28:02  albert
c Modified so that string values in the FITS header be no less than 8
c characters long as required by the FITS standard.
c
c Revision 1.3  1992/06/15  18:13:16  albert
c Added code to write the zenith angles to the output FITS header.
c
c Revision 1.2  1992/04/16  16:57:32  albert
c Modified to use the new FITS keywords.
c
c Revision 1.1  1992/01/29  19:54:11  albert
c Initial revision
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC wrtfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      subroutine wrtfts(cfilnm,sign,intflg,ocntbn,oexpbn,ointbn,*)
C
C      Integer     	Sign,Inx,I,J,K,P,Num,Totcnt,Cdate(3),Fscale
C      Integer*2   	Cntbuf(1440),Expbuf(1440),Intbuf(1440)
C      Logical	  	Intflg
C      Character   	Cfilnm*80,Efilnm*80,Ifilnm*80,Coment*47
C      Character   	Id*80,Ctype1*8,Ctype2*8,String*24,Strng2*8,Frmt*4
C      Double Precision  Conv,Zenm
C      Equivalence (Obuffr(1),Cntbuf(1)),(Obuffr(2),Expbuf(1))
C      Equivalence (Obuffr(3),Intbuf(1))
C
C      Include 'Oufits.Cmn.F'
C
C      Real Ocntbn(Onaxs1,Onaxs2,Onaxs3)
C      Real Oexpbn(Onaxs1,Onaxs2,Onaxs3)
C      Real Ointbn(Onaxs1,Onaxs2,Onaxs3)
C
C      Common /Id/ Id
C      Data Num/1/
C
CC---> Initialize Variables
C      Id = '$id$'
C      If (Intflg) Num = 4
C      Conv = 45.0/Atan(1.0)
C      Ocdel1 = Ocdel1 * Conv
C      Ocrvl1 = Ocrvl1 * Conv + Ocdel1/2
C      If (Ocrvl1 .Gt. 180) Ocrvl1 = Ocrvl1 - 360
C      Ocdel2 = Ocdel2 * Conv
C      Ocrvl2 = Ocrvl2 * Conv + Ocdel2/2
C      Totcnt = 0
C
CC---> Set The Fits Keywords for the coordinate system
C      if (ocoord .eq. 'GALA') then
C	 ctype1 = 'GLON    '
C	 ctype2 = 'GLAT    '
C      else if (ocoord .eq. 'CELE') then
C	 ctype1 = 'RA      '
C	 ctype2 = 'DEC     '
C      else if (ocoord .eq. 'ERTH') then
C	 ctype1 = 'LATD    '
C	 ctype2 = 'LONG    '
C      else if (ocoord .eq. 'INST') then
C	 ctype1 = 'TETX    '
C	 ctype2 = 'TETY    '
C      else if (ocoord .eq. 'TGAL') then
C	 ctype1 = 'GTLON   '
C	 ctype2 = 'GTLAT   '
C      else if (ocoord .eq. 'TCEL') then
C	 ctype1 = 'CTRA    '
C	 ctype2 = 'CTDEC   '
C      else if (ocoord .eq. 'SUN') then
C	 ctype1 = 'SLON    '
C	 ctype2 = 'SLAT    '
C      else if (ocoord .eq. 'MOON') then
C	 ctype1 = 'MLON    '
C	 ctype2 = 'MLAT    '
C      end if
C
CC---> Open the output FITS files
C      call fopen(2, 2, cfilnm, 0, 'FB', 2880, 2880)
C      obuffr(1) = ' '
C      if (intflg) then
C	 i = index(cfilnm,'counts')
C	 if (i .le. 0) i = index(cfilnm,' ')
C	 efilnm = cfilnm
C	 efilnm(i:i+5) = 'exposr'
C         call fopen(2, 20, efilnm, 0, 'FB', 2880, 2880)
C         obuffr(2) = ' '
C	 ifilnm = cfilnm
C	 ifilnm(i:i+5) = 'intens'
C         call fopen(2, 30, ifilnm, 0, 'FB', 2880, 2880)
C         obuffr(3) = ' '
C      end if
C
C      write(3,'(//,'' FITS file header generated:'',/)')
C
CC---> Write the rectangular map header
C      if (ogridt.eq.'RECT') then
C	 coment = 'Standard FITS format'
C         call wrtlin(num,'SIMPLE  ',' ','l1   ',' ',.true.,0.0,coment)
C         coment = 'Number of bits per pixel (2-byte integers)'
C         call wrtlin(num,'BITPIX  ',' ','i2   ',' ',16,0.0,coment)
C         coment = 'Number of axis in primary array'
C         call wrtlin(num,'NAXIS   ',' ','i1   ',' ',onaxis,0.0,coment)
C         coment = 'Number of bins per row'
C         call wrtlin(num,'NAXIS1  ',' ','i3   ',' ',onaxs1,0.0,coment)
C         coment = 'Number of rows'
C         call wrtlin(num,'NAXIS2  ',' ','i3   ',' ',onaxs2,0.0,coment)
C         coment = 'Number of energy levels'
C         call wrtlin(num,'NAXIS3  ',' ','i3   ',' ',onaxs3,0.0,coment)
C         coment = 'Min coordinate of reference bin (RA, GLON)'
C         call wrtlin(num,'CRVAL1  ',' ','f7.2 ',' ',0,ocrvl1,coment)
C         coment = 'Location of reference point in array'
C         call wrtlin(num,'CRPIX1  ',' ','f7.2 ',' ',0,dreal(ocrpx1),coment)
C         coment = 'Interval in degrees'
C         call wrtlin(num,'CDELT1  ',' ','f7.2 ',' ',0,ocdel1,coment)
C         coment = 'Coordinate type'
C         call wrtlin(num,'CTYPE1  ',' ','a8   ',ctype1,0,0.0,coment)
C         coment = 'Min coordinate of reference bin (RA, GLON)'
C         call wrtlin(num,'CRVAL2  ',' ','f7.2 ',' ',0,ocrvl2,coment)
C         coment = 'Location of reference point in array'
C         call wrtlin(num,'CRPIX2  ',' ','f7.2 ',' ',0,dreal(ocrpx2),coment)
C         coment = 'Interval in degrees'
C         call wrtlin(num,'CDELT2  ',' ','f7.2 ',' ',0,ocdel2,coment)
C         coment = 'Coordinate type'
C         call wrtlin(num,'CTYPE2  ',' ','a8   ',ctype2,0,0.0,coment)
C
CC---> Write the Aitoff map header
C      else
C         coment = 'Non-standard format for Aitoff maps'
C         call wrtlin(num,'SIMPLE  ',' ','l1   ',' ',.false.,0.0,coment)
C         coment = 'Number of bits per pixel (2-byte integers)'
C         call wrtlin(num,'BITPIX  ',' ','i2   ',' ',16,0.0,coment)
C         coment = 'Number of axis in primary array'
C         call wrtlin(num,'NAXIS   ',' ','i1   ',' ',4,0.0,coment)
C         coment = 'Special format for Aitoff map'
C         call wrtlin(num,'NAXIS1  ',' ','i3   ',' ',0,0.0,coment)
C         coment = 'Number of bins on 1st axis'
C         call wrtlin(num,'NAXIS2  ',' ','i3   ',' ',onaxs1,0.0,coment)
C         coment = 'Number of bins on 2nd axis'
C         call wrtlin(num,'NAXIS3  ',' ','i3   ',' ',onaxs2,0.0,coment)
C         coment = 'Number of energy levels'
C         call wrtlin(num,'NAXIS4  ',' ','i3   ',' ',onaxs3,0.0,coment)
C         coment = 'Min coordinate of reference bin (RA, GLON)'
C         call wrtlin(num,'CRVAL2  ',' ','f7.2 ',' ',0,ocrvl1,coment)
C         coment = 'Location of reference point in array'
C         call wrtlin(num,'CRPIX2  ',' ','f7.2 ',' ',0,dreal(ocrpx1),coment)
C         coment = 'Interval in degrees'
C         call wrtlin(num,'CDELT2  ',' ','f7.2 ',' ',0,ocdel1,coment)
C         coment = 'Coordinate type'
C         call wrtlin(num,'CTYPE2  ',' ','a8   ',ctype1,0,0.0,coment)
C         coment = 'Min coordinate of reference bin (DEC, GLAT)'
C         call wrtlin(num,'CRVAL3  ',' ','f7.2 ',' ',0,ocrvl2,coment)
C         coment = 'Location of reference point in array'
C         call wrtlin(num,'CRPIX3  ',' ','f7.2 ',' ',0,dreal(ocrpx2),coment)
C         coment = 'Interval in degrees'
C         call wrtlin(num,'CDELT3  ',' ','f7.2 ',' ',0,ocdel2,coment)
C         coment = 'Coordinate type'
C         call wrtlin(num,'CTYPE3  ',' ','a8   ',ctype2,0,0.0,coment)
C      endif
C
CC---> Write the counts data type and conversion factors
C      coment = 'Units of primary array'
C      call wrtlin(1,'BUNIT   ',' ','a8   ','COUNTS  ',0,0.0,coment)
C      coment = 'Data scaling (none)'
C      call wrtlin(1,'BSCALE  ',' ','e13.7',' ',0,dreal(obscal(1)),coment)
C      coment = 'Data offset (none)'
C      call wrtlin(1,'BZERO   ',' ','e13.7',' ',0,0.0d0,coment)
C
CC---> Write the intensity and exposure data type and conversion factors
C      if (intflg) then
C         coment = 'Units of primary array'
C         call wrtlin(2,'BUNIT   ',' ','a8   ','EXPOSURE',0,0.0,coment)
C         coment = 'Data scaling'
C         call wrtlin(2,'BSCALE  ',' ','e13.7',' ',0,dreal(obscal(2)),coment)
C         coment = 'Data offset'
C         call wrtlin(2,'BZERO   ',' ','e13.7',' ',0,dreal(obzero(2)),coment)
C         coment = 'Units of primary array'
C         call wrtlin(3,'BUNIT   ',' ','a9   ','INTENSITY',0,0.0,coment)
C         coment = 'Data scaling'
C         call wrtlin(3,'BSCALE  ',' ','e13.7',' ',0,dreal(obscal(3)),coment)
C         coment = 'Data offset'
C         call wrtlin(3,'BZERO   ',' ','e13.7',' ',0,dreal(obzero(3)),coment)
C      end if
C
CC---> Write additional Aitoff map parameters
C      if (ogridt.eq.'AITF') then
C         coment = 'For Aitoff plots, data is in groups'
C         call wrtlin(num,'GROUPS  ',' ','l1   ',' ',.true.,0.0,coment)
C         coment = 'Number of parameters for each group'
C         call wrtlin(num,'PCOUNT  ',' ','i1   ',' ',oprcnt,0.0,coment)
C         coment = 'Number of groups'
C         call wrtlin(num,'GCOUNT  ',' ','i4   ',' ',ogrcnt,0.0,coment)
C      endif
C
CC---> Indicate that the reference point is at the center of the pixel
C      coment = 'The reference point is at the pixel center'
C      call wrtlin(num,'PIXCENT ',' ','l1   ',' ',.true.,0.0,coment)
C 
CC---> Write information saved from the very 1st input file header
C      string = '********* Following info'
C      coment = 'rmation saved from the 1st input file ********'
C      call wrtlin(num,'HISTORY ',' ','free ',string,0,0.0,coment)
C      do j=1,oindex(1)
C  	 if (oheadr(1,j)(1:5).ne.'FILEA'.and.oheadr(1,j)(1:5).ne.'FILEB') then
C            strng2 = oheadr(1,j)(1:8)
C            string = oheadr(1,j)(9:32)
C            coment = oheadr(1,j)(33:79)
C            call wrtlin(num,strng2,' ','free ',string,0,0.0,coment)
C  	 end if
C      end do
C      string = '********* End of informa'
C      coment = 'tion saved from the 1st input file ***********'
C      call wrtlin(num,'HISTORY ',' ','free ',string,0,0.0,coment)
C
CC---> Write the energy levels
C      j = 0
C      coment = 'Minimum energy of map in level xx (Mev)'
C      do i=1,onaxs3
C         j = j + 1
C         write(strng2,'(a6,i1,a1)') 'MINENG',j,' '
C         coment(2:3) = 'in'
C         if (i.eq.10) write(strng2,'(a6,i2)') 'MINENG',j
C         write(coment(32:33),'(i2)') j
C         call wrtlin(num,strng2,' ','i6   ',' ',int(oenrgy(1,i)),0.0,coment)
C         strng2(2:3) = 'AX'
C         coment(2:3) = 'ax'
C         call wrtlin(num,strng2,' ','i6   ',' ',int(oenrgy(2,i)),0.0,coment)
C      end do
C
C      coment = 'Maximum angle from zenith in level xx'
C      do i=1,onaxs3
C	 zenm = oznmax(i)
C         write(strng2,'(a6,i1,a1)') 'ZENMAX',i,' '
C         if (i.eq.10) write(strng2,'(a6,i2)') 'ZENMAX',i
C         write(coment(36:37),'(i2)') i
C         call wrtlin(num,strng2,' ','f9.3 ',' ',0,zenm,coment)
C      end do
C
Cc---> write additional header information
C      call idate(cdate)
C      write(string,'(i2.2,a1,i2.2,a1,i2.2)') cdate(1),'/',cdate(2),
C     &   '/',mod(cdate(3),100)
C      coment = 'Current date in dd/mm/yy'
C      call wrtlin(num,'DATE    ',' ','a8   ',string,0,0.0,coment)
C      coment = 'Map type of the primary array data'
C      if (ogridt.eq.'RECT') then
C         call wrtlin(num,'PRIMTYPE',' ','a8   ','RECT_MAP',0,0.0,coment)
C      else if (ogridt.eq.'AITF') then
C         call wrtlin(num,'PRIMTYPE',' ','a10  ','AITOFF_MAP',0,0.0,coment)
C      end if
C      coment = 'Data type in the file'
C      call wrtlin(1,'FILETYPE',' ','a16  ','EGRET_COUNTS_MAP',0,0.0,coment)
C      call wrtlin(2,'FILETYPE',' ','a16  ','EGRET_EXPOSR_MAP',0,0.0,coment)
C      call wrtlin(3,'FILETYPE',' ','a16  ','EGRET_INTENS_MAP',0,0.0,coment)
C      string = 'SUM_OF_MAPS'
C      coment = 'Addition of 2 maps'
C      if (sign .eq. -1) then
C         coment = 'Subtraction of 2 maps'
C	 string(3:3) = 'B'
C      end if
C      call wrtlin(num,'MAPTYPE ',' ','a11  ',string,0,0.0,coment)
C      coment = 'Number of elements in the map'
C      call wrtlin(num,'MAPSIZE ',' ','i6   ',' ',omapsz,0.0,coment)
C      coment = 'File produced at Goddard Space Flight Center'
C      call wrtlin(num,'ORIGIN  ',' ','a8   ','GSFC    ',0,0.0,coment)
C      coment = 'Compton Observatory (Gamma Ray Observatory)'
C      call wrtlin(num,'TELESCOP',' ','a8   ','GRO     ',0,0.0,coment)
C      coment = 'The GRO instrument is EGRET'
C      call wrtlin(num,'INSTRUME',' ','a8   ','EGRET   ',0,0.0,coment)
C      coment = 'Original FITS file name on EGRET Suns'
C      i = index(cfilnm, ' ') - 1
CC      call wrtlin(1,'FILENAME',' ','a18   ',cfilnm(i-17:i),0,0.,coment)
CC      call wrtlin(2,'FILENAME',' ','a18   ',efilnm(i-17:i),0,0.,coment)
CC      call wrtlin(3,'FILENAME',' ','a18   ',ifilnm(i-17:i),0,0.,coment)
C      call wrtlin(1,'FILENAME',' ','a18   ',cfilnm(i-10:i),0,0.,coment)
C      call wrtlin(2,'FILENAME',' ','a18   ',efilnm(i-10:i),0,0.,coment)
C      call wrtlin(3,'FILENAME',' ','a18   ',ifilnm(i-10:i),0,0.,coment)
C      coment = 'PI is Carl Fichtel'
C      call wrtlin(num,'OBSERVER',' ','a8   ','FICHTEL ',0,0.0,coment)
C      coment = 'Name of object or nominal pointing id'
C      call wrtlin(num,'OBJECT1 ',' ','a8   ','        ',0,0.0,coment)
C      coment = 'Name used for file cataloging & archiveing'
C      call wrtlin(num,'FILE-ID ',' ','a8   ','        ',0,0.0,coment)
C      coment = 'Version of file used for cataloging'
C      call wrtlin(num,'FILE-VER',' ','a8   ','        ',0,0.0,coment)
C
CC---> Write the names of the files added to this map
C      runnum = runnum + 1
C      if (runnum .lt. 10) frmt = '(i1)'
C      if (runnum .ge. 10) frmt = '(i2)'
C      if (runnum .ge. 100) frmt = '(i3)'
C      do i=1,2
C         do j=1,oindex(i)
C	    if (oheadr(i,j)(1:5).eq.'FILEA'.or.oheadr(i,j)(1:5).eq.'FILEB') then
C               if (oheadr(i,j)(6:8) .eq. '000') then
C	          write(oheadr(i,j)(6:8), frmt) runnum
C	          write(oheadr(i,j)(75:77), frmt) runnum
C	       end if
C               strng2 = oheadr(i,j)(1:8)
C               string = oheadr(i,j)(9:32)
C               coment = oheadr(i,j)(33:79)
C               call wrtlin(num,strng2,' ','free ',string,0,0.0,coment)
C	    end if
C         end do
C      end do
C
CC---> Write the output file names
C      call wrtlin(num,'END     ',' ','a1   ',' ',0,0.0,' ')
C      inx = 1
C
CC---> Write the rectangular map data to the file
C      if (ogridt.eq.'RECT') then
C
CC------> Loop over all the bins, copy to the buffer, write the buffer when full
C         do k=1,onaxs3
C            do j=1,onaxs2
C               do i=1,onaxs1
C                  cntbuf(inx) = (ocntbn(i,j,k)-obzero(1))/obscal(1)
C                  expbuf(inx) = (oexpbn(i,j,k)-obzero(2))/obscal(2)
C                  intbuf(inx) = (ointbn(i,j,k)-obzero(3))/obscal(3)
C		  totcnt = totcnt + cntbuf(inx) 
C                  inx = inx + 1
C                  if (inx.gt.1440) then
C                     call fwrite(cntbuf,2,2880)
C		     if (intflg) then
C                        call fwrite(expbuf,20,2880)
C                        call fwrite(intbuf,30,2880)
C		     end if
C                     inx = 1
C                  endif
C               end do
C            end do
C         end do
C
CC---> Do the Aitoff maps
C      else if (ogridt.eq.'AITF') then
C
CC------> Loop over the energy levels
C         do k=1,onaxs3
C
CC---------> Loop over the latitudes
C            do j=1,onaxs2
C
CC------------> Write the group parameters for each group
C	       fscale = 1
C               do p=1,oprcnt
C                  cntbuf(inx) = oftprm(p,j) * fscale
C                  expbuf(inx) = cntbuf(inx)
C                  intbuf(inx) = cntbuf(inx)
C                  inx = inx + 1
C                  if (inx.gt.1440) then
C                     call fwrite(cntbuf,2,2880)
C		     if (intflg) then
C                        call fwrite(expbuf,20,2880)
C                        call fwrite(intbuf,30,2880)
C                     end if
C                     inx = 1
C                  endif
C		  fscale = 100
C	       end do
C
CC------------> Loop over the longitudes
C               do i=1,oftprm(1,j)
C
CC---------------> Copy the bin data to the FITS record. write when full
C                  cntbuf(inx) = (ocntbn(i,j,k)-obzero(1))/obscal(1)
C                  expbuf(inx) = (oexpbn(i,j,k)-obzero(2))/obscal(2)
C                  intbuf(inx) = (ointbn(i,j,k)-obzero(3))/obscal(3)
C		  totcnt = totcnt + cntbuf(inx) 
C                  inx = inx + 1
C                  if (inx.gt.1440) then
C                     call fwrite(cntbuf,2,2880)
C		     if (intflg) then
C                        call fwrite(expbuf,20,2880)
C                        call fwrite(intbuf,30,2880)
C		     end if
C                     inx = 1
C                  endif
C
C               end do
C            end do
C         end do
C      endif
C
CC---> Write the last buffer if necessary
C      if (inx.gt.1) then
C         call fwrite(cntbuf,2,2880)
C         if (intflg) then
C            call fwrite(expbuf,20,2880)
C            call fwrite(intbuf,30,2880)
C	 end if
C      end if
C
CC---> Close the output file(s)
C      call fclose(2)
C      if (intflg) then
C         call fclose(20)
C         call fclose(30)
C      end if
C      write(3,'(//,''The true total number of counts written to the file is'',
C     &		f11.2)') totcnt*obscal(1)
C
C      return
C
C1000  format(a5,2(2x,a17),1x,2(f6.1),2x,a3,2x,a4,2(3x,'f'),a121,/,
C     &       76x,a120)
C2000  format(i2.2,'/',i2.2,'/',i2.2,',',i2.2,':',i2.2,':',i2.2,'.',i3.3)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End wrtfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      end
C
C
C
C
C
C
C
C      This is the end of the old ERGET code.  Below is the new program.
C
C
C
C
C
C
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC nwrtfts.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine nwrtfts(cfilnm,sign,intflg,ocntbn,oexpbn,ointbn,
     & status,*)
      implicit none 

      integer           sign,i
      logical	  	intflg
      character   	cfilnm*80,efilnm*80,ifilnm*80

      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'

      real ocntbn(onaxs1,onaxs2,onaxs3)
      real oexpbn(onaxs1,onaxs2,onaxs3)
      real ointbn(onaxs1,onaxs2,onaxs3)
      double precision  conv
C
C      New variable added for fitsio
C
    
      integer status,blocksize,group
      integer lun_cnt,lun_exp,lun_int
C
C  All fitsio routines use "inherited status" convention.  This means that 
C  if an error has already occurred (i.e. status has a value other than 0)
C  then a subroutine will exit immediately without changing the value of
C  status.   This convention will be maintained for the EGRET I/O.  (See 
C  section 4.6 of the FITSIO User's Guide Version 5.0 by William Pence)
C
       if (status .gt. 0 )  then 
           return
       endif
C
C    J. Silvis HSTX 
C     6 Aug 1997  
C
C
C    Delete the file if it already exists so that we can recreate it.
C
        call deletefile(cfilnm,status)

C
C     Get an unused Logical Unit Number to use to open the FITS file
C
       call ftgiou(lun_cnt,status)
       if (intflg) then
          call ftgiou(lun_exp,status)
          call ftgiou(lun_int,status)
       endif
C
C     create the new empty FITS file  
C     blocksize=1 means that a 2880-byte logical record will 
C     be used
C
       blocksize=1
       call ftinit(lun_cnt,cfilnm,blocksize,status)
C
C        If the intensity flag is set make file name for
C        the exposure times and intensity files and open the 
C        files.
C
       if (intflg) then
         i = index(cfilnm,'counts')
         if (i .le. 0) i = index(cfilnm,' ')
         efilnm = cfilnm
         efilnm(i:i+5) = 'exposr'
         ifilnm = cfilnm
         ifilnm(i:i+5) = 'intens'
         call deletefile(efilnm,status)
         call deletefile(ifilnm,status)
         call ftinit(lun_int,ifilnm,blocksize,status)
         call ftinit(lun_exp,efilnm,blocksize,status)
       endif
C
C   Initialize global variable to be written into the header
C
 
      conv = 45.0/atan(1.0)
      ocdel1 = ocdel1 * conv
      ocrvl1 = ocrvl1 * conv + ocdel1/2
      if (ocrvl1 .gt. 180) ocrvl1 = ocrvl1 - 360
      ocdel2 = ocdel2 * conv
      ocrvl2 = ocrvl2 * conv + ocdel2/2
C
C     write the header keywords
C
      call  wrthd(lun_cnt,sign,1,cfilnm,status)

      if (intflg) then
       call  wrthd(lun_exp,sign,2,efilnm,status)
       call  wrthd(lun_int,sign,3,ifilnm,status)
      endif
      group = 1
       call ftp3de(lun_cnt,group,onaxs1,onaxs2,
     &       onaxs1,onaxs2,onaxs3,ocntbn,status)  
C
C      Store the data if the intensity and exposure files have been 
C      created.
C
       if (intflg) then
          call ftp3de(lun_int,group,onaxs1,onaxs2,
     &       onaxs1,onaxs2,onaxs3,ointbn,status)  
          call ftp3de(lun_exp,group,onaxs1,onaxs2,
     &       onaxs1,onaxs2,onaxs3,oexpbn,status)  

      endif 
C
C     close the file and free the unit number
C
       call ftclos(lun_cnt, status)
       call ftfiou(lun_cnt, status)
C
C   Close the logical files and free logical if the intensity 
C   and exposure data have been stored.
C
       if (intflg) then
          call ftclos(lun_int, status)
          call ftfiou(lun_int, status)
          call ftclos(lun_exp, status)
          call ftfiou(lun_exp, status)
       endif
       return
       end
