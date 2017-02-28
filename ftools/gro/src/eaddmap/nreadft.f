CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC readft.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  readft
CH1
CH1  Ftool conversion complete 8 Aug 1997
CH1  A number of major changes to this code were made to convert it to an Ftool
CH1  See comments below.
CH1  
CH1  Jeff Silvis HSTX
CH1
CH1  $Id: nreadft.f,v 1.7 2005/08/26 19:36:34 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Reads a FITS format file. Decodes the file header, calls
CH1            chkfts to check that the header information is compatible
CH1            with the program, calls readat to read the bin data.
CH1	       If the user requested the intensity map then the corresponding
CH1	       exposure map is read.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  
CH2     Call  nreadft(lunout,in,filnam,intflg,maxsiz,icntbn,iexpbn,status,*)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	in	    I*4	    I	Index of the input file number (1 or 2)
CH2	filnam	    CH*80   I	Name of the counts intput file (including path)
CH2	intflg	    L*4     I	User requested the intensity file flag
CH2     icntbn      I*4         Counts map bin values (360,200,10)
CH2     iexpbn      R*4         Exposure map bin values
C*********************************************************************************
C    Ftool change  New variable added
C
C    lunout        I*4         gives a logical number for the ascii output file
C                               (see below)
C    status         I*4         gives the error status of the fitsio functions
C  
C    J. Silvis HSTX 
C     6 Aug 1997  
C
C**********************************************************************************
CH2	*			Return line to goto in case of error
CH2
CH2  Called by:  eaddmap
CH2
CH2  Calls:
CH2*******************************************************************************
CH2   Subroutines no longer used by Ftool:
CH2   fopen : System routine to open a file to be used by FTIO
CH2   fread : FTIO system routine to read a sequential file
CH2   fclose: System routine to close an FTIO file
CH2*******************************************************************************
CH2
CH2   chkfts: Checks that the FITS file read is compatible with the program
CH2   nreadat: Reads the actual FITS data
CH2   ftgiou: allocates a logical number that does not conflict with fitsio
CH2   ftfiou: frees a logical number
CH2   ftopen: opens a fits file
CH2   ftclos: closes a fits file
CH2   ftghsp: finds the number of keywords in a header
CH2   ftgrec: gets a 80 character header from the header
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
CH3 itotcn     Integer   Total number of counts in input map
CH3 imaxvl(2)  Real      Maximum counts bin value for the input maps
CH3 iminvl(2)  Real      Minimum counts bin value for the input maps
CH3 ixmaxp(2)  Real      X position of the maximum bin value
CH3 iymaxp(2)  Real      Y position of the maximum bin value
CH3 izmaxp(2)  Integer   Z index for the maximum bin value
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
CH3  linpos     I*4        1      Next position in the line for read
CH3  length     I*4        -      Length of the FITS record read
CH3  maxlev	I*4	  10	  Maximum number of energy levels
CH3  numlev     I*4        0      Number of energy levels counted
CH3  numzen     I*4        0      Number of zenith angle values
CH3  simple     L*4        F      Determines if data is in simple format
CH3  groups     L*4        F      Determines if data is in groups
CH3  keywrd     Ch*8       -      Current keyword in line
CH3  keywr2     Ch*8       -      Current secondary keyword in line
CH3  line       Ch*80      -      Current line from header
CH3  filnm2	Ch*80	   -	  Name of the exposure file
CH3  buffer(2)  Ch*2880    -	  Record buffer for the niput files
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4
CH4  Method:
CH4     Initialize variables for the FITS file
CH4     Find the number of keywords in the header
CH4     Loop over the number of keywords 
CH4        Read the next FITS record
CH4        while (the end of the record has not been found) do
CH4           Get the next line from the record read
CH4           Decode the keyword of the line
CH4           If (the keyword is recognized) then
CH4              Save the corresponding value in the appropriate variable
CH4              If (the keyword is BUNIT) save the position in the rec
CH4           Else
CH4              Save the line in a buffer
CH4           Endif
CH4        End while
CH4     End loop
CH4     Call chkfts to check the file header
CH4	If (intensity map requested) repeat processing for the exposure file
CH4     Call readat to read the bin data
CH4	Write information out the files read
CH4  End readft
CH4
CH5 $Log: nreadft.f,v $
CH5 Revision 1.7  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.6  1999/03/25 21:20:26  toliver
CH5 removed dead code causing compiler warnings
CH5
c Revision 1.5  1997/11/16  21:20:04  silvis
c Several changes were made to make the code compatible with OSF.  Most of these
c changes involved changing the print format statements and modifying the max
c and min functions.
c
c Jeff Silvis
c
CH5 Revision 1.4  1997/11/03 22:55:52  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:19  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:59:04  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:16:01  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C*****************************************************************************
C           Ftool Changes for readft
C
C Readft reads the header then calls readat to read the data portion of 
C a fits file.  All of this was done with nonstandard i/o and has been 
C replaced with fitsio.
C
C     I.  Lun_out for error i/o
C
C The variable lunout added to the argument call. This variable
C provides a logicial number that is used for output to an ascii file.
C In the original code, this was hardwired to be 3.  Since this is
C now an Ftool, the logical number 3 is used by XPI to link with the
C parameter file, so the hardwiring causes a conflict.  A value for
C lunout was assigned by the fitsio routine ftgiou in the main
C routine, eaddamp.  Then lunout is added as an argument to any
C routine, such as this one, that has  output to the ascii file.
C
C     II. Use status to track fitsio errors (more details below)
C
C     III. Switch from using buffers to logical numbers
C 
C The egret routine fread took the entire fits file, both the header and 
C the data and read it into a single character buffer. Now using the fitsio
C routines ftgiou and ftopen, a logical number is assinged to the fits file.
C Later other fitsio routines operate on the fits file using this logical.
C
C     IV. Switch from gotos to loops
C
C Originally the program went through the header porton line by line until
C it hit an end statement.  This was done using goto statements.  Now the 
C fitsio routine ftghsp finds the number of lines in the header.  This number
C is used to set up a loop to read the header.
C
C     V. Reading the header
C
C Originally lines were read off the buffer.  Now the fitsio routine ftgrec
C reads a line from the header into the character variable, "line".  The 
C character "line" is broken up in the same way as in the original code.  As
C in the original code the portions of the header that will be sent to the 
C output header file are first stored in the common array oheadr.  Later 
C wrtfts will take the contents of oheadr and write it to the output header.
C
C     VI. Changes to readat
C
C Readat is a subroutine within readft that reads the data portion of
C the fits file.  In the past character buffers were passed to it.  Now
C logical numbers are sent.  There are several other changes in this code.
C See the documentation on readat for more information.  
C
C  Jeff Silvis
C
C  Hughes STX
C  
C Changes completed 6 June 1997
C
C******************************************************************************
C
C Revision 1.12  1995/01/04  17:43:35  albert
C Modified to allow up to 999 viewing periods to be added instead of only 99
C
C Revision 1.11  1994/12/14  14:10:44  albert
C Modified to search for string 'counts' instead of just '.' when trying to
C generate the exposure file name from the counts file name.
C
C Revision 1.10  1994/02/04  15:08:04  albert
C Read the input map type and set a flag to ignore the cutoff angle if the
C map is a sum (or subtraction) of maps.
C
C Revision 1.9  1994/01/28  20:49:33  albert
C Modified to get the pointing direction and the cutoff angle from the input
C file header.
C
C Revision 1.8  1993/06/24  19:05:50  albert
C Made reading the FITS header independent of the energy levels sequence
C
C Revision 1.7  1993/05/24  14:05:41  albert
C Processed the total number of counts as real since counts are now reals.
C Put the input map in (-180, 180) system.
C
C Revision 1.6  1993/05/07  13:56:47  albert
C Corrected typo in variable name
C
C Revision 1.5  1993/05/05  13:08:08  albert
C Modified to read maps with the bin center convention.
C
C Revision 1.4  1993/02/22  14:30:33  albert
C Modified so that only certain header entries of the 1st input FITS file are
C saved, that the file names of all input files are saved and that the current
C run number (how many times input file 1 was added) is computed.
C
C Revision 1.3  1992/08/27  15:34:27  albert
C Modified to greatly increase the number of maps that may be added by only
C saving the name of the input files in the output file after 2 addmap
C have been run on a file.
C
C Revision 1.2  1992/04/16  16:49:31  albert
C Modified to read the new FITS keywords.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC readft.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine nreadft(lunout,in,filnam,intflg,maxsiz,icntbn,iexpbn,
     & status,*)
      implicit none
      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'
      integer lunout
      real		icntbn(*),iexpbn(*),ptdir(4)
C************************************************************************
C     Ftool change
C
C      The variable lenght and linpos are no longer needed.  They are used 
C      to read through the character buffer.
C      integer   	linpos,length,maxlev,i,in,i1,numlev,inaxs4,numzen
C
C************************************************************************
      integer   	maxlev,i,in,i1,numlev,inaxs4,numzen
      integer		maxsiz,numsav,rnum,ilev,detmax
      logical   	intflg,simple,groups,toomny,pixcnt
      character 	filnam*80,line*80,keywrd*8,keywr2*8,ctype1*4,ctype2*4
C**************************************************************************
C
C  FTOOL Change
C
C The fits file is no longer read into a buffer.  Rather fitsio is 
C is used, so buffer is no longer used by the code.
C
C  Jeff Silvis 
C 
C  Sept 1997
C      character 	filnm2*80,id*80,buffer(2)*2880,ctype3*4
C**************************************************************************
      character 	filnm2*80,ctype3*4
       character        id*80
      character		type(3)*9,tempgr*16,maptyp*20
      double precision  icrvl3,icrpx3,icdel3,twopi,conv
C***************************************************************************
C     Ftool change
C
C     The variables below had to be added to the program to make use of 
C     fitsio.

C     J Silvis
C****************************************************************************
      integer rec_index,nkeys,nspace,lun_cnt,lun_exp,status
      integer blocksize,readwrite

C**************************************************************************
C
C  FTOOL Change
C
C The fits file is no longer read into a buffer.  Rather fitsio is 
C is used, so the line below is no longer used by the code.
C
C  Jeff Silvis 
C 
C  Sept 1997
C
C      equivalence (obuffr(1),buffer)
C
C**************************************************************************
 
      common /id/id
      data   type /'COUNTS', 'EXPOSURE','INTENSITY'/, toomny/.false./
      id= '$Id: nreadft.f,v 1.7 2005/08/26 19:36:34 irby Exp $'

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
       if (status .gt. 0 )  then 
           return
       endif
C
C    J. Silvis HSTX 
C     6 Aug 1997  
C
C
C***************************************************************************
C---> Initialize the FITS parameters before reading the file
      twopi     = atan(1.0) * 8
      conv      = twopi / 360.0
      simple    = .false.
      groups    = .false.
      pixcnt    = .false.
      inaxis    = 0
      inaxs1    = 0
      inaxs2    = 0
      inaxs3    = 0
      numlev    = 0
      numzen    = 0
      numsav    = 0
      maxlev    = 10
      ibitpx(1) = 0
      ibscal(1) = 1
      ibzero(1) = 0
      itotcn    = 0.0
      maptyp    = ' '
      do i=1,2
	 imaxvl(i) = -1.0e30
	 iminvl(i) = +1.0e30
      end do

C---> Open input FITS file
      write(lunout,'(///,79(''-''))')
      write(lunout,*) 'Header of input FITS file number',in,filnam
C****************************************************************************
C
C            Ftool Change:
C   The non-standard fits reader below is replaced with fitsio.
C   The code below reads the fits files that contains the counts data.
C   Instead of having the errors printed by using gotos to either
C   line 2000 or 3000, the status flag from fitsio will be written
C   to the log file using the routine: ftstatus_wrt.f.     
C
C
C      call fopen(1, 1, filnam, 0, 'FB', 2880, 2880)
C
C---> Read the FITS file header
C10    continue
C      call fread(buffer(1),1,length,*2000,*3000)
C
C      linpos = 1
C
C
C      Jeff Silvis 9 May 1997
C
C
C   Now for the fitsio:
C  **********************************************************************
      status = 0
      readwrite = 0
      call ftgiou(lun_cnt,status)
      call ftopen(lun_cnt,filnam,readwrite,blocksize,status)
C
C     Find the number of keywords in the header.
C
      call ftghsp(lun_cnt,nkeys,nspace,status)
      keywrd = ' '
      keywr2 = ' '
C*********************************************************************
C
C            Ftool Change:
C  Instead of using gotos  to loop, use nkeys to set up a do loop.
C
C  J. Silvis   
C
C---> Loop until the end of the header is found
C 20    continue
C
C*********************************************************************
      Do rec_index=1, nkeys
C     if (keywrd.ne.'END') then
C*********************************************************************
C            Ftool Change:
C   Replace the code below that reads from the buffer with fitsio.
C   
C   J. Silvis
C
C         line = buffer(1)(linpos:linpos+79)
C**********************************************************************
         call ftgrec(lun_cnt,rec_index,line,status)
         keywrd = line(1:8)
C------> Decode the FITS keyword (goto used because depth "if" too big)
         if (keywrd.eq.'HISTORY') goto 35
         if (keywrd(1:1).eq.'C') goto 30

         if (keywrd.eq.'SIMPLE') then
            read(line(11:30),*) simple
         else if (keywrd.eq.'BITPIX') then
            read(line(11:30),*) ibitpx(1)
         else if (keywrd.eq.'NAXIS') then
            read(line(11:30),*) inaxis
         else if (keywrd.eq.'NAXIS1') then
            read(line(11:30),*) inaxs1
         else if (keywrd.eq.'NAXIS2') then
            read(line(11:30),*) inaxs2
         else if (keywrd.eq.'NAXIS3') then
            read(line(11:30),*) inaxs3
         else if (keywrd.eq.'NAXIS4') then
            read(line(11:30),*) inaxs4
         else if (keywrd.eq.'BUNIT') then
            read(line(11:30),*) ibunit(1)
         else if (keywrd.eq.'BSCALE') then
            read(line(11:30),*) ibscal(1)
         else if (keywrd.eq.'BZERO') then
            read(line(11:30),*) ibzero(1)
         else if (keywrd.eq.'GROUPS') then
            read(line(11:30),*) groups
         else if (keywrd.eq.'PCOUNT') then
            read(line(11:30),*) iprcnt
         else if (keywrd.eq.'GCOUNT') then
            read(line(11:30),*) igrcnt
         else if (keywrd.eq.'PIXCENT') then
            read(line(11:30),*) pixcnt
         else if (keywrd.eq.'MAPTYPE'.and.maptyp.ne.'NOCUTOFF') then
            read(line(11:30),*) maptyp
	    if (maptyp.eq.'SUM_OF_MAPS' .or. maptyp.eq.'SUB_OF_MAPS') 
     &	       maptyp = 'NOCUTOFF'
	 else if (keywrd.eq.'PRIMTYPE') then
	    read(line(11:30),*) tempgr
	    igridt = tempgr(1:4)
            if (igridt .eq. 'AITO') igridt = 'AITF'
         else if (keywrd(1:6).eq.'MINENG') then
            read(keywrd(7:8),'(i2)') ilev
            if (ilev .gt. maxlev) goto 6000
            read(line(11:30),*) ienrgy(1,ilev)
            numlev = max(numlev,ilev)
         else if (keywrd(1:6).eq.'MAXENG') then
            read(keywrd(7:8),'(i2)') ilev
            if (ilev .gt. maxlev) goto 6000
            read(line(11:30),*) ienrgy(2,ilev)
            numlev = max(numlev,ilev)
	 else if (keywrd(1:6).eq.'ZENMAX') then
            read(keywrd(7:8),'(i2)') ilev
            read(line(11:30),*) iznmax(ilev)
            numzen = max(numzen,ilev)
         else if (keywrd.eq.'SC-Z-RA') then
            read(line(11:30),*) ptdir(3)
	    ptdir(3) = ptdir(3) * conv
         else if (keywrd.eq.'SC-Z-DEC') then
            read(line(11:30),*) ptdir(4)
	    ptdir(4) = ptdir(4) * conv
         else if (keywrd.eq.'SC-Z-BII') then
            read(line(11:30),*) ptdir(2)
	    ptdir(2) = ptdir(2) * conv
         else if (keywrd.eq.'SC-Z-LII') then
            read(line(11:30),*) ptdir(1)
	    ptdir(1) = ptdir(1) * conv
         else if (keywrd.eq.'DETMAX') then
            read(line(11:30),*) detmax
         else if (keywrd.ne.'END') then
	    if (keywrd.eq.'FILENAME') then
	       oindex(in) = oindex(in) + 1
               write(oheadr(in,oindex(in)),10000) char(in+64),
     &           line(11:30),in
	    else if (keywrd(1:5).eq.'FILEA'.or.
     &             keywrd(1:5).eq.'FILEB') then
	       oindex(in) = oindex(in) + 1
	       oheadr(in,oindex(in)) = line(1:80)
	       read(line(6:8),'(i3)') rnum
	       if (rnum .gt. runnum) runnum = rnum
	    else if (in .eq. 1 .and. .not.toomny) then
	       if (keywrd(1:5).eq.'STRT-'.or.keywrd(1:4).eq.'END-'.or.
     &	       keywrd.eq.'OBS-TIME'.or.keywrd(1:5).eq.'DATE-'.or.
     &	       keywrd(1:5).eq.'TIME-'.or.keywrd.eq.'EQUINOX'.or.
     &         keywrd(1:3).eq.'SC-'.or.keywrd.eq.'INSTRA1'.or.
     &         keywrd.eq.'INSTDEC1') then
	          oindex(in) = oindex(in) + 1
	          oheadr(in,oindex(in)) = line(1:80)
	       endif
	    endif
	    if (oindex(1)+oindex(2).gt.999 .or. runnum.ge.999)
     &         toomny = .true.
C
C****************************************************************************
C
C    Ftool Change
C      The write line below will not work on Solaris.  It has been 
C   replaced withe the simpler command below.
C
C        J. Silvis  
C
C         26 Aug 1997
C
C
C            write(line(80:80),*) '*'
C
C********************************************************
           line(80:80) = '*'
         endif
         goto 40

C------> Decode the keywords that begin with the letter 'C'
30       continue
         if (keywrd.eq.'CRVAL1') then
            read(line(11:30),*) icrvl1
	    icrvl1 = icrvl1 * conv
         else if (keywrd.eq.'CRPIX1') then
            read(line(11:30),*) icrpx1
         else if (keywrd.eq.'CDELT1') then
            read(line(11:30),*) icdel1
	    icdel1 = icdel1 * conv
         else if (keywrd.eq.'CTYPE1') then
            read(line(11:30),*) ctype1
         else if (keywrd.eq.'CRVAL2') then
            read(line(11:30),*) icrvl2
	    icrvl2 = icrvl2 * conv
         else if (keywrd.eq.'CRPIX2') then
            read(line(11:30),*) icrpx2
         else if (keywrd.eq.'CDELT2') then
            read(line(11:30),*) icdel2
	    icdel2 = icdel2 * conv
         else if (keywrd.eq.'CTYPE2') then
            read(line(11:30),*) ctype2
         else if (keywrd.eq.'CRVAL3') then
            read(line(11:30),*) icrvl3
	    icrvl3 = icrvl3 * conv
         else if (keywrd.eq.'CRPIX3') then
            read(line(11:30),*) icrpx3
         else if (keywrd.eq.'CDELT3') then
            read(line(11:30),*) icdel3
	    icdel3 = icdel3 * conv
         else if (keywrd.eq.'CTYPE3') then
            read(line(11:30),*) ctype3
         else
C
C****************************************************************************
C
C    Ftool Change
C      The write line below will not work on Solaris.  It has been 
C   replaced withe the simpler command below.
C
C        J. Silvis  
C
C         26 Aug 1997
C
C
C            write(line(80:80),*) '*'
C
C********************************************************
           line(80:80) = '*'
         endif
         goto 40

C------> Decode the history keywords
35       continue
         keywr2 = line(9:16)
         if (keywr2.eq.'GRID') then
            read(line(19:43),*) igridt
         else if (keywr2(1:4).eq.'ENRG') then
            numlev = numlev + 1
            if (numlev .gt. maxlev) goto 6000
            read(line(20:30),*) ienrgy(1,numlev),ienrgy(2,numlev)
	 else if (keywr2(1:4).eq.'ZENM') then
	    numzen = numzen + 1
	    read(line(20:43),*) iznmax(numzen)
         else
	    if (in .eq. 1 .and. .not.toomny .and. keywr2.ne.'********')
     &         then
	       oindex(in) = oindex(in) + 1
	       oheadr(in,oindex(in)) = line(1:80)
	    end if
C
C****************************************************************************
C
C    Ftool Change
C      The write line below will not work on Solaris.  It has been 
C   replaced withe the simpler command below.
C
C        J. Silvis  
C
C         26 Aug 1997
C
C
C            write(line(80:80),*) '*'
C
C********************************************************
           line(80:80) = '*'
         endif

C------> Increment the indexes for the next line
40       continue
         write(lunout,*) line
C*************************************************************************
C            Ftool Change:
C
C  Before fitsio was added the lines below were used to terminate
C  the loop and increment the variable linpos.  This is now done 
C  with the end do
C
C
C  J Silvis 9 May 1997
C
C         linpos = linpos + 80
C         if (linpos.lt.2880) goto 20
C         if (keywrd.ne.'END') goto 10
C*****************************************************************************
C       endif
      end do
C---> Test for special condition when inaxs1 = 0
      if (inaxs1.eq.0) then
         inaxis = inaxis - 1
         inaxs1 = inaxs2
         inaxs2 = inaxs3
         inaxs3 = inaxs4
         ctype1 = ctype2
         ctype2 = ctype3
         icrvl1 = icrvl2
         icrvl2 = icrvl3
         icdel1 = icdel2
         icdel2 = icdel3
         icrpx1 = icrpx2
         icrpx2 = icrpx3
      endif

C---> If the reference point is at the center of the pixel, adjust the
C---> reference coordinates to be at the bottom left corner which is the default
      if (pixcnt) then
         icrvl1 = icrvl1 - icdel1/2
         if (icrvl1 .lt. -twopi/2) icrvl1 = icrvl1 + twopi
         icrvl2 = icrvl2 - icdel2/2
         if (icrvl2 .lt. -twopi/4) then
            write(lunout,*) 
     &       'ERROR: latitude reference point is < -90. Set to -90'
            icrvl2 = -twopi/4
         end if
      end if
      if (icrvl1 .gt. twopi/2) icrvl1 = icrvl1 - twopi

C---> Call chkfts to check the FITS file information
      toomny = .true.
      call chkfts(lunout,simple,groups,numlev,ctype1,ctype2,
     &  maxlev,maxsiz,ptdir,*9000)
C---  > If an intensity file is requested, the exposure file must be read too
      if (intflg) then
      	 ibitpx(2) = 0
         ibscal(2) = 1
         ibzero(2) = 0
	 i = index(filnam,'counts')
         if (i .le. 0) i = index(filnam,' ')
	 filnm2 = filnam
	 filnm2(i:i+5) = 'exposr'
   22 format(/,"Reading the exposure file number",i2,":"/a)   
      write(lunout,22)in,filnm2
C****************************************************************************
C
C            Ftool Change:
C   The non-standard fits reader below is replaced with fitsio.
C   The code below reads the fits files that contains the counts data.
C   Instead of having the errors printed by using gotos to either
C   line 2000 or 3000, the status flag from fitsio will be written
C   to the log file using the routine: ftstatus_wrt.f.
C
C
C      call fopen(1, 10, filnm2, 0, 'FB', 2880, 2880)
C------> Read the FITS file header
C 110      continue
C         call fread(buffer(2),10,length,*2000,*3000)
C      
C      linpos = 1
C
C
C      Jeff Silvis 23 May 1997
C
C   Now for the fitsio:
C****************************************************************************
         status = 0
         readwrite=0
         call ftgiou(lun_exp,status)
         call ftopen(lun_exp,filnm2,readwrite,blocksize,status)
C
C     Find the number of keywords in the header.
C
         call ftghsp(lun_exp,nkeys,nspace,status)
                                                
         keywrd = ' '

C*********************************************************************
C
C            Ftool Change:
C  Instead of using gotos  to loop, use nkeys to set up a do loop.
C
C  J. Silvis
C
C---> Loop until the end of the header is found
C 120    continue
C
C*********************************************************************
         Do rec_index=1, nkeys
C         if (keywrd.ne.'END') then 
C*********************************************************************
C            Ftool Change:
C   Replace the code below that reads from the buffer with fitsio.
C
C   J. Silvis
C
C         line = buffer(2)(linpos:linpos+79)
C**********************************************************************
            call ftgrec(lun_exp,rec_index,line,status)
            keywrd = line(1:8)
            if (keywrd.eq.'BITPIX') then
               read(line(11:30),*) ibitpx(2)
            else if (keywrd.eq.'BUNIT') then
               read(line(11:30),*) ibunit(2)
	       if (ibunit(2).ne.'EXPOSURE') then
		  write(lunout,*) 'readft - error: data type is not exposure',
     &		     ibunit(2)
		  goto 9000
	       end if
            else if (keywrd.eq.'BSCALE') then
               read(line(11:30),*) ibscal(2)
            else if (keywrd.eq.'BZERO') then
               read(line(11:30),*) ibzero(2)
            endif

C*************************************************************************
C            Ftool Change:
C
C  Before fitsio was added the lines below were used to terminate
C  the loop and increment the variable linpos.  This is now done
C  with the end do
C
C
C  J Silvis 23 May 1997
C
C         linpos = linpos + 80
C         if (linpos.lt.2880) goto 120
C         if (keywrd.ne.'END') goto 110
C         endif
C               The endif above closed the removed line:
C             C         if (keywrd.ne.'END') then
C               from the begining of this loop.
C
C
*****************************************************************************
          end do
        endif
C****************************************************************************
C  Ftool change
C
C  Replace the old version of readat with a new version that uses fitsio.
C  This new version of readat has logical units passed to it instead of 
C  buffers.
C
C  J. Silvis 13 May 1997
C
C---> Call readat to read the FITS array data
C      call readat(lunout,buffer(1),buffer(2),intflg,icntbn,iexpbn,ptdir,detmax
C     &    ,maptyp,*9000)
C
C ***************************************************************************
        call nreadat(lunout,lun_cnt,lun_exp,intflg,icntbn,
     &    iexpbn,ptdir,detmax,maptyp,status,*9000)
C
C
C**************************************************************************
C
C                  Ftool Change
C
C   The close statements below must be changed to fitsio
C
C
C     J Silvis
C
C
C
C---> Close the input files
C      call fclose(1)
C     if (intflg) call fclose(10)
C
C*****************************************************************************
      call ftclos (lun_cnt,status)
      call ftfiou (lun_cnt,status)
      if (intflg) then 
           call ftclos (lun_exp,status)
           call ftfiou (lun_exp,status)
      endif           
C---> Write information about the input map
      i1 = 1
      if (intflg) i1 = 2
      write(lunout,'(/,''Information about the file(s) read:'',/)')
      write(lunout,*) 'Data Type      Max Bin',       
     &   'Location of Max (x,y,z)        Min Bin'
      do i=1,i1
         if (igridt.eq.'AITF') icdel1 = twopi/inax12(int(iymaxp(i)))
	 ixmaxp(i) = (icrvl1 + icdel1*(ixmaxp(i)-icrpx1+0.5)) / conv
	 iymaxp(i) = (icrvl2 + icdel2*(iymaxp(i)-icrpx2+0.5)) / conv
	 write(lunout,'(a9,4x,e13.7,4x,2(f7.2,2x),i2,5x,e13.7)')
     &      type(i),imaxvl(i),ixmaxp(i),iymaxp(i),izmaxp(i),iminvl(i)
      end do
      write(lunout,'(/,''Total number of counts ='',f9.2)') itotcn

      return

C2000  write(lunout,*) 'READFT: premature end of file in the FITS file'
C      return 1
C3000  write(lunout,*) 'READFT: I/O error in reading the FITS file'
C      return 1
6000  write(lunout,*) 'READFT: too many energy levels. Limit to',maxlev
9000  return 1

10000 format('FILE',a1,'000= ',a20,' / Name of input file ',i1,
     &       ' added in run number 000   ')
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End readft.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end








