CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(READFT) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  READFT
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: readft.f,v 1.5 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Reads a FITS format file. Decodes the file header, calls
CH1            CHKFTS to check that the header information is compatible
CH1            with the program, calls IMREADAT to read the bin data.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call READFT(ftsFile)
CH2  Argument	Type	I/O                 Description
CH2  --------	----	---	---------------------------------------------
CH2  ftsFile	C*80	I	Name of fits file to read.
CH2
CH2  Called by:  INTMAP
CH2
CH2  Calls:
CH2   AECON : EGRET utility routine to convert between ASCII and EBCDIC
CH2   CHKFTS: Checks if the FITS header information is correct
CH2   CONVRT: EGRET utility routine to convert a TJD/MSD time in an R*8
CH2   FREAD : System routine to read a record
CH2   GRDTJD: EGRET utility routine to convert times from cal to TJD/MSD
CH2   IMREADAT: Reads the FITS bin data
CH2   REWIND: System routine to position at the beginning of a file
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
CH3 buffer(2)  Ch*2880   FITS record buffer (may hold up to 2 header rc)
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
CH3  linpos     I*4        1      Next position in the line for read
CH3  length     I*4        -      Length of the FITS record read
CH3  tjd        I*4        -      Data time truncated Julian day
CH3  msd        I*4        -      Data time millisecond of day
CH3  numlev     I*4        0      Number of energy levels counted
CH3  simple     L*4        F      Determines if data is in simple format
CH3  groups     L*4        F      Determines if data is in groups
CH3  line       Ch*80      -      Current line from header
CH3  keywrd     Ch*8       -      Current keyword in line
CH3  bunit      Ch*8       -      Data type
CH3  maptyp     Ch*4       -      Map type (single point., disk etc)
CH3  y2kdate    L*4        F      Determines if date format is yyyy-mm-dd
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       1              Input FITS file
CH4       6              Printer report
CH4
CH4  Method:
CH4     Initialize variables for the FITS file
CH4     While (the end of the header has not been found) do
CH4        Read the next FITS record
CH4        Convert the record to EBCDIC
CH4        while (the end of the record has not been found) do
CH4           Get the next line from the record read
CH4           Decode the keyword of the line
CH4           If (the keyword is recognized) then
CH4              Save the corresponding value in the appropriate variabl
CH4              If (the keyword is BUNIT) save the position in the rec
CH4           Else
CH4              Write a warning message on the report file
CH4           Endif
CH4        End while
CH4     End while
CH4     Save the position of the last line in the header
CH4     Call CHKFTS to check the file header
CH4     Call IMREADAT to read the bin data
CH4  End READFT
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Added parameter ftsFile
CH5				fOpened FITS file before reading.
CH5				Commented out AECON; not needed on SUN.
CH5				Merged changes from version 1.03 on IBM:
CH5				Remove code to get header pointers.
CH5                             Read the earth cutoff angles and
CH5                             the maximum angle from the detector
CH5                             axis from the FITS header.
CH5
CH5 $Log: readft.f,v $
CH5 Revision 1.5  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.4  2006/04/17 20:54:27  irby
CH5 Updates for compliance with g95 and gfortran:
CH5
CH5 - Replace call to lnblnk with equivalent call to len_trim.
CH5   lnblnk (a routine which used to live in libg2c) is not currently
CH5   available with g95 or gfortran.
CH5
CH5 - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
CH5
CH5 - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
CH5
CH5 - Fix non-integer loop variables.
CH5
CH5 Revision 1.3  2002/12/26 17:16:32  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:04  irby
CH5 New GRO tool intmap.
CH5
c
c Linux version  2001/08/08  D.L.Bertsch.  Main change is in the calls
c     to fread, fopen, and fclose 
c
c Revision 2.9  1999/03/16  18:45:21  lmm
c checks date format to determine if it is in new Y2K format
c
c Revision 2.8  1998/02/24  18:49:48  wang
c  Fix the y2k problems.
c
c Revision 2.7  1993/06/24  19:03:45  albert
c Made reading the FITS file header independent of the energy levels sequence
c
c Revision 2.6  1993/05/07  13:51:47  albert
c Corrected typo in variable name
c
c Revision 2.5  1993/05/05  12:54:44  albert
c Modified to accept input maps that use the bin center convention
c
c Revision 2.4  1992/10/14  16:28:27  albert
c Modified to read tand store the exclude times from the counts map header.
c
c Revision 2.3  1992/06/02  19:21:58  albert
c Corrected problem in reading the event class in some cases.
c
c Revision 2.2  1992/04/01  21:58:14  albert
c Used variable dimension arrays read in as parameter input. Used the new
c set of FITS keywords. Added code to read the exposure map if requested.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

Cesp  Subroutine READFT
      Subroutine READFT(data_dir,ftsFile,convert,cntbin,expbin)

      character*(*) data_dir, ftsFile

      real          expbin(*),crval3,crpix3,cdelt3
      real*8        CONVRT
      integer       linpos,length,tjd,msd,mm,dd,yy,hr,mn,sc,mil,ret
      integer       cntbin(*),numlev,naxis4,numzen,itemp,ilev,i,convert
c      integer       ind1,ind2
      logical       simple,groups,pixcnt,y2kdate
      character(80)  line, expFile, fname
      character(16)  tempgr,tempmt
      character(8)   keywrd, keywr2, bunit
      character(4)   ctype1, ctype2, ctype3, maptyp

      integer   status, fread, fopen, fclose

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'

      character(80)	id

      save

      common	/id/id
      id = '$Id: readft.f,v 1.5 2013/05/21 19:08:24 irby Exp $'


C---> Initialize the FITS parameters before reading the file
      simple = .false.
      groups = .false.
      pixcnt = .false.
      y2kdate = .false.
      bitpix = 0
      naxis  = 0
      naxis1 = 0
      naxis2 = 0
      naxis3 = 0
      bscale(1) = 1
      bzero(1)  = 0
      numlev = 0
      numzen = 0
      strtim = 0
      endtim = 0
      evclas = 1
      detmax = 40
      maxlev = 10
      numexc = 0

Cesp  ! First open the FITS file !
c     call fopen(1, 1, ftsFile, 0, 'FB', 2880, 2880)

c     Change for Linux
      fname = data_dir(1:len_trim(data_dir)) // ftsFile

      status = fopen(1, 1, fname, 0, 'FB', 2880, 2880)
      
      write(6,'(///,'' header of the input FITS file:'',/)')

C---> Read the FITS file header
 10   continue
c     Call FREAD(buffer(1),1,length,*2000,*3000)

c     Change for Linux
      status = fread(buffer(1),1,length)

      if( status.eq.1 ) go to 2000
      if( status.eq.2 ) go to 3000


Cesp  ! ASCII to EBCDIC conversion not needed; SUN is native ASCII !
Cesp  Call AECON(1,buffer(1),2880,ret)
Cesp  if (ret.ne.0) write(6,*) 'READFT: number ASCII-EBCDIC errors=',ret

      linpos = 1
      keywrd = ' '
      keywr2 = ' '

C---> Loop until the end of the header is found
 20   continue
      if (keywrd.ne.'END') then
         line = buffer(1)(linpos:linpos+79)
Cae      write(6,*) line
         keywrd = line(1:8)

C------> Decode the FITS keyword (goto used because depth "if" too big)
         if (keywrd.eq.'HISTORY') goto 35
         if (keywrd(1:1).eq.'C') goto 30

         if (keywrd.eq.'SIMPLE') then
            read(line(11:30),*) simple
         else if (keywrd.eq.'BITPIX') then
            read(line(11:30),*) bitpix
         else if (keywrd.eq.'NAXIS') then
            read(line(11:30),*) naxis
         else if (keywrd.eq.'NAXIS1') then
            read(line(11:30),*) naxis1
         else if (keywrd.eq.'NAXIS2') then
            read(line(11:30),*) naxis2
         else if (keywrd.eq.'NAXIS3') then
            read(line(11:30),*) naxis3
         else if (keywrd.eq.'NAXIS4') then
            read(line(11:30),*) naxis4
         else if (keywrd.eq.'BUNIT') then
            read(line(11:30),*) bunit
         else if (keywrd.eq.'BSCALE') then
            read(line(11:30),*) bscale(1)
         else if (keywrd.eq.'BZERO') then
            read(line(11:30),*) bzero(1)
         else if (keywrd.eq.'GROUPS') then
            read(line(11:30),*) groups
         else if (keywrd.eq.'PCOUNT') then
            read(line(11:30),*) pcount
         else if (keywrd.eq.'GCOUNT') then
            read(line(11:30),*) gcount
         else if (keywrd.eq.'PIXCENT') then
            read(line(11:30),*) pixcnt
         else if (keywrd.eq.'DATE-OBS') then
            if (line(14:14) .eq. '/') then
               read(line(12:30),'(3(i2,1x))') dd,mm,yy
            else
               read(line(12:30),'(i4,1x,i2,1x,i2,1x)') yy,mm,dd
               y2kdate = .true.
            endif
         else if (keywrd.eq.'TIME-OBS') then
            read(line(12:30),'(3(i2,1x),i3)') hr,mn,sc,mil
Clmm            if (yy .le. 1998) then 
            if (.not.y2kdate) then 
               call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,ret)
            else 
               call GRDTJD(tjd,msd,yy,mm,dd,hr,mn,sc,mil,ret)
            endif
            if (ret.ne.0) goto 5000
            strtim = CONVRT(tjd,msd)
         else if (keywrd.eq.'DATE-END') then
            if (line(14:14) .eq. '/') then
               read(line(12:30),'(3(i2,1x))') dd,mm,yy
            else
               read(line(12:30),'(i4,1x,i2,1x,i2,1x)') yy,mm,dd
            endif
         else if (keywrd.eq.'TIME-END') then
            read(line(12:30),'(3(i2,1x),i3)') hr,mn,sc,mil
Clmm            if (yy .le. 1998) then 
            if (.not.y2kdate) then 
               call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,ret)
            else 
               call GRDTJD(tjd,msd,yy,mm,dd,hr,mn,sc,mil,ret)
            endif
            if (ret.ne.0) goto 5000
            endtim = CONVRT(tjd,msd)
         else if (keywrd.eq.'PRIMTYPE') then
            read(line(11:30),*) tempgr
	    gridtp = tempgr(1:4)
	    if (gridtp .eq. 'AITO') gridtp = 'AITF'
         else if (keywrd.eq.'MAPTYPE') then
            read(line(11:30),*) tempmt
	    maptyp = tempmt(1:4)
	    if (maptyp .eq. 'SING') maptyp = 'SNGL'
	    if (maptyp .eq. 'ALL_') maptyp = 'ASKY'
	    if (maptyp .eq. 'GALA') maptyp = 'GALD'
         else if (keywrd(1:6).eq.'MINENG') then
            read(keywrd(7:8),'(i2)') ilev
            if (ilev .gt. maxlev) goto 6000
            read(line(11:30),*) energy(1,ilev)
	    numlev = max(numlev,ilev)
         else if (keywrd(1:6).eq.'MAXENG') then
            read(keywrd(7:8),'(i2)') ilev
            if (ilev .gt. maxlev) goto 6000
            read(line(11:30),*) energy(2,ilev)
	    numlev = max(numlev,ilev)
	 else if (keywrd(1:6).eq.'ZENMAX') then
            read(keywrd(7:8),'(i2)') ilev
	    read(line(11:30),*) zenmax(ilev)
	    numzen = max(numzen,ilev)
	 else if (keywrd.eq.'DETMAX') then
	    read(line(11:30),*) detmax
         else if (keywrd.eq.'E-RETCOD') then
            read(line(12:12),'(i1)') evclas
         else if (keywrd.ne.'END') then
c	    write(line(80:80),*) '*'
         endif
         goto 40

C------> Decode the keywords that begin with the letter 'C'
 30      continue
         if (keywrd.eq.'CRVAL1') then
            read(line(11:30),*) crval1
         else if (keywrd.eq.'CRPIX1') then
            read(line(11:30),*) crpix1
         else if (keywrd.eq.'CDELT1') then
            read(line(11:30),*) cdelt1
         else if (keywrd.eq.'CTYPE1') then
            read(line(11:30),*) ctype1
         else if (keywrd.eq.'CRVAL2') then
            read(line(11:30),*) crval2
         else if (keywrd.eq.'CRPIX2') then
            read(line(11:30),*) crpix2
         else if (keywrd.eq.'CDELT2') then
            read(line(11:30),*) cdelt2
         else if (keywrd.eq.'CTYPE2') then
            read(line(11:30),*) ctype2
         else if (keywrd.eq.'CRVAL3') then
            read(line(11:30),*) crval3
         else if (keywrd.eq.'CRPIX3') then
            read(line(11:30),*) crpix3
         else if (keywrd.eq.'CDELT3') then
            read(line(11:30),*) cdelt3
         else if (keywrd.eq.'CTYPE3') then
            read(line(11:30),*) ctype3
         else
c	    write(line(80:80),*) '*'
         endif
         goto 40

C------> Decode the history keywords
 35      continue
         keywr2 = line(9:16)
         if (keywr2.eq.'GRID') then
            read(line(19:43),*) gridtp
         else if (keywr2.eq.'MAPTYPE') then
            read(line(19:43),*) maptyp
         else if (keywr2.eq.'S TIME') then
            read(line(20:40),'(6(i2,1x),i3)') mm,dd,yy,hr,mn,sc,mil
            if (yy .le. 65) yy = yy + 100
            call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,ret)
            if (ret.ne.0) goto 5000
            strtim = CONVRT(tjd,msd)
         else if (keywr2.eq.'E TIME') then
            read(line(20:40),'(6(i2,1x),i3)') mm,dd,yy,hr,mn,sc,mil
            if (yy .le. 65) yy = yy + 100
            call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,ret)
            if (ret.ne.0) goto 5000
            endtim = CONVRT(tjd,msd)
         else if (keywr2(1:4).eq.'ENRG') then
            numlev = numlev + 1
            if (numlev .gt. maxlev) goto 6000
            read(line(20:30),*) energy(1,numlev),energy(2,numlev)
	 else if (keywr2(1:4).eq.'ZENM') then
	    numzen = numzen + 1
	    read(line(20:43),*) zenmax(numzen)
	 else if (keywr2.eq.'DETMAX') then
	    read(line(20:43),*) detmax
         else if (keywr2.eq.'E RETCOD') then
            read(line(20:32),'(i1)') evclas
         else if (keywr2.eq.'EXCLUDE '.or.keywr2.eq.'CALIBRAT'.or.
     &           keywr2.eq.'ALBEDO M'.or.keywr2.eq.'TEST MOD') then
            read(line(19:39),'(6(i2,1x),i3)') dd,mm,yy,hr,mn,sc,mil
            if (yy .le. 65) yy = yy + 100
            call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,ret)
            if (ret.ne.0) goto 5000
            strexc(numexc+1) = CONVRT(tjd,msd)
            read(line(43:63),'(6(i2,1x),i3)') dd,mm,yy,hr,mn,sc,mil
            if (yy .le. 65) yy = yy + 100
            call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,ret)
            if (ret.ne.0) goto 5000
	    numexc = numexc + 1
            endexc(numexc) = CONVRT(tjd,msd)
         else
c	    write(line(80:80),*) '*'
         endif

C------> Increment the indexes for the next line
 40      continue
         write(6,*) line
         linpos = linpos + 80
         if (linpos.lt.2880) goto 20
         if (keywrd.ne.'END') goto 10
      endif


C---> Test for special condition when naxis1 = 0
      write(6,'(/,''(*) Line not used'')')
      if (naxis1.eq.0) then
         naxis = naxis - 1
         naxis1 = naxis2
         naxis2 = naxis3
         naxis3 = naxis4
         ctype1 = ctype2
         ctype2 = ctype3
         crval1 = crval2
         crval2 = crval3
         cdelt1 = cdelt2
         cdelt2 = cdelt3
         crpix1 = crpix2
         crpix2 = crpix3
      endif

C---> If the reference point is at the center of the pixel, adjust the
C---> reference coordinates to be at the bottom left corner which is the default
      if (pixcnt) then
         crval1 = crval1 - cdelt1/2
         if (crval1 .lt. -180) crval1 = crval1 + 360
         crval2 = crval2 - cdelt2/2
         if (crval2 .lt. -90) then
            write(6,*)'ERROR: latitude ref. point is < -90. Set to -90'
            crval2 = -90
         end if
      end if

C---> Call CHKFTS to check the FITS file information
      call CHKFTS(simple,bunit,groups,numlev,ctype1,ctype2,maptyp)
      if (retcod.ne.0) return

C---> If the exposure file exists and should be used, read it
      if (usexpf) then
         bscale(2) = 1
         bzero(2) = 0
         expFile = fname

         i = len_trim(expFile) - 22
         expFile(i:i+5) = 'exposr'
	 write(6,'(/,''Reading the exposure file: '',a)') expFile
c     call fopen(1, 3, expFile, 0, 'FB', 2880, 2880)

c     Change for Linux
         status = fopen(1, 3, expFile, 0, 'FB', 2880, 2880)

C------> Read the FITS file header
 110     continue
c     Call FREAD(buffer(2),3,length,*2000,*3000)

         status = fread(buffer(2),3,length)

c     Change for Linux
csb         status = fread(buffer(1),1,length)
         if( status.eq.1 ) go to 2000
         if( status.eq.2 ) go to 3000

         linpos = 1
         keywrd = ' '

C------> Loop until the end of the header is found
 120     continue
         if (keywrd.ne.'END') then
            line = buffer(2)(linpos:linpos+79)
            keywrd = line(1:8)
            if (keywrd.eq.'BITPIX') then
               read(line(11:30),*) itemp
	       if (itemp.ne.bitpix) then
		  write(6,*) 'READFT: bad bitpix in exposure file'
		  retcod = 8
	       end if
            else if (keywrd.eq.'BUNIT') then
               read(line(11:30),*) tempgr
	       if (tempgr.ne.'EXPOSURE') then
		  write(6,*) 'READFT: data type is not "EXPOSURE"'
		  retcod = 8
	       end if
            else if (keywrd.eq.'BSCALE') then
               read(line(11:30),*) bscale(2)
            else if (keywrd.eq.'BZERO') then
               read(line(11:30),*) bzero(2)
            endif

C---------> Increment the indexes for the next line
            linpos = linpos + 80
            if (linpos.lt.2880) goto 120
            if (keywrd.ne.'END') goto 110
	 endif
      endif

C---> Call IMREADAT to read the FITS array data
      call IMREADAT(convert,cntbin,expbin)
      
      call rewind(1)
c     if (usexpf) call fclose(3)

c     Change for Linux
      if (usexpf) status = fclose(3)

      return

 2000 write(6,*) 'READFT: premature end of file in the FITS file'
      retcod = 8
      return
 3000 write(6,*) 'READFT: I/O error in reading the FITS file'
      retcod = 12
      return
 5000 write(6,*) 'READFT: invalid time:',tjd,msd,mm,dd,yy,hr,mn,sc,mil
      retcod = 8
      return
 6000 write(6,*) 'READFT: too many energy levels. Limit to',maxlev
      retcod = 8
      return

CCCCCCCCCCCCCCCCCCCC END SKYMAP.SOURCE(READFT) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
