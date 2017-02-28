CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(WRTLIN) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  WRTLIN
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: wrtlin.f,v 1.4 2013/05/21 19:08:25 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Generates 1 FITS header line.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call WRTLIN(h,p,keywd1,keywd2,frmt,char,int,rel,
CH2                                 coment)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     h           I*4    I/O  header array index for current record
CH2     p           I*4    I/O  Line position in current header record
CH2     keywd1      Ch*8    I   Primary FITS header keyword
CH2     keywd2      Ch*8    I   Secondary keyword for history keyword
CH2     frmt        Ch*5    I   Fortran I/O format for value to write
CH2     char        Ch*24   I   Value of the keyword if character used
CH2     int         I*4     I   Value of the keyword if integer used
CH2     rel         R*4     I   Value of the keyword if real used
CH2     coment      Ch*47   I   Comment for the line to write
CH2
CH2  Called by:  WRTFTS
CH2
CH2  Calls: None
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
CH3  COMMON Block Name: GLOBAL (Holds the main variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 strtim     Real*8    FITS data start time (TJD & MSD combined)
CH3 endtim     Real*8    FITS data end time (TJD & MSD combined)
CH3 retcod     Integer   Program return code (0 = normal)
CH3 specin(10) Real      Spectral indexes
CH3 walldi     Integer   Wall distance (from SAGE Namelist)
CH3 maxlev     Integer   Maximum number of energy levels (10)
CH3 tascco     Integer   TASC in coincidence flag
CH3 acs        Integer   ACS value
CH3 tunit      Integer   Unit number for timeline file
CH3 eunit      Integer   Unit number for exposure history file
CH3 calfil(2)  Ch*8      Names of the calibration files
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3   oldh       I*4       0       Previous value of h
CH3   flen       I*4       -       Length of the value to write
CH3   xlen       I*4       -       Number of x spaces to write
CH3   f          Ch*1      -       Type of value to write
CH3   formt      Ch*21    (A2,     Format to use when writing the line
CH3   key2       Ch*8      -       Secondary keyword
CH3   fr         Ch*5      -       Format of the value to write
CH3   string     Ch*24     -       String of keyword and value to write
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       6              Printer report
CH4
CH4  Method:
CH4     Find the length of the value to write
CH4     If (the value is of character type) allow 2 more spaces (for '')
CH4     Compute the number of x spaces to leave after the keyword
CH4     Compose the format of the line to be written
CH4     Write the keyword on the output line
CH4     If (there is a secondary keyword) write the secondary keyword
CH4     If (the keyword = 'END') then
CH4        Write 'END' on the output line
CH4     Else if (the value to write is a string) then
CH4        Append quotes around the string value
CH4        Write the string value and the comment on the output line
CH4     Else if (the value to write is an integer) then
CH4        Write the integer value and the comment on the output line
CH4     Else if (the value to write is a real) then
CH4        Write the real value and the comment on the output line
CH4     Else if (the value to write is a logical) then
CH4        Write the logical value and the comment on the output line
CH4     Endif
CH4     If (the current record is full or the keyword = 'END') then
CH4        Increment the record index
CH4        Reset the line index to 1
CH4     Endif
CH4  End WRTLIN
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Merged changes of version 1.03 on IBM:
CH5				Changed the routine so that it writes to a line
CH5				instead of writing to a FITS record.
CH5
CH5 $Log: wrtlin.f,v $
CH5 Revision 1.4  2013/05/21 19:08:25  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.3  2002/12/26 17:16:32  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.2  2002/04/18 19:37:57  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:04  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine WRTLIN(line,keywd1,keywd2,frmt,char,int,rel,coment)

      real       rel
      integer    j,int,flen,xlen
      character    f
      character(5)  frmt,fr
      character(8)  keywd1,keywd2,key2
      character(21) formt
      character(24) char, string
      character(47) coment
      character(80) line

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'

      data formt/'(A2,'/

      character(80)	id
      common	/id/id
      id = '$Id: wrtlin.f,v 1.4 2013/05/21 19:08:25 irby Exp $'

C---> Save the format of the value to write and the secondary keyword
      fr = frmt
      f  = ' '
      key2 = keywd2
      if (key2(1:1).eq.' ') key2 = ' '

C---> Get the length of the value to write from the format
      if (fr(3:3).ge.'0'.and.fr(3:3).le.'9') then
         read(fr(2:3),'(I2)') flen
      else
         read(fr(2:2),'(I1)') flen
      endif

C---> If the value is a character, increase the length by 2 (for the '')
      f = fr(1:1)
      if (f.eq.'A') then
         flen = flen + 2
         write(fr(2:3),'(I2.2)') flen
      endif

C---> Compute the # of spaces to leave and generate the format code
      xlen = 20 - flen
      formt(14:21) = ',A3,A47)'
      if (key2.ne.' ') then
         xlen = xlen + 5
         formt(14:21) = ',A3,A34)'
      endif
      if (f.eq.'A') then
         write(formt(5:13),'(A5,A1,I2,A1)') fr,',',xlen,'X'
      else
         write(formt(5:13),'(I2,A2,A5)') xlen,'X,',fr
      endif

C---> Write the keyword for the current line
      write(line(1:8),'(A8)') keywd1
      j = 9

C---> Write the secondary keyword (if any)
      if (key2.ne.' ') then
	  write(line(j:16),'(A8)') key2
	  j = 17
      endif

C---> Write the value for the keyword depending on its data type
      if (keywd1.eq.'END') then
	 write(line,'(A3,A77)') 'END',' '
      else if (f.eq.'A') then
         string = ''''//char
         string(flen:flen) = ''''
	 write(line(j:80),formt) '= ',string,' / ',coment
      else if (f.eq.'I') then
	 write(line(j:80),formt) '= ',int,' / ',coment
      else if (f.eq.'F'.or.f.eq.'E') then
	 write(line(j:80),formt) '= ',rel,' / ',coment
      else if (f.eq.'L') then
	 write(line(j:80),formt) '= ',int,' / ',coment
      else if (f.eq.' ') then
         write(line(j:80),*) 'HISTORY ',char,coment
      endif

      return

CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(WRTLIN) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
