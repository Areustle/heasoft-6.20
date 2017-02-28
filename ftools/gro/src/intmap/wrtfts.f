CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(WRTFTS) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  WRTFTS
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: wrtfts.f,v 1.5 2013/05/21 19:08:25 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Writes the exposure and intensity maps into a FITS
CH1            format file.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call WRTFTS(counts)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	counts	c*(*)	I	Name of the counts file.
CH2
CH2  Called by:  INTMAP
CH2
CH2  Calls:
CH2   AECON : EGRET utility routine to convert between ASCII and EBCDIC
CH2   FREAD : System routine to read a record
CH2   FWRITE: System routine to write a record
CH2   WRTLIN: Writes a header line
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
CH3  exgbin     R*4        -      Exposure bins data for galactic disk
CH3  (594,66,10)
CH3  ingbin     R*4        -      Intensity bins data for galactic disk
CH3  (594,66,10)
CH3  inx        I*4        1      Index into the FITS buffer
CH3  g          I*4        -      Group index
CH3  p          I*4        -      Position in header or parameter index
CH3  h          I*4        -      Index for array of header buffers
CH3  expbuf     I*2        -      Exposure map output buffer
CH3  (1440)
CH3  intbuf     I*2        -      Intensity map output buffer
CH3  (1440)
CH3  cmt        Ch*47      -      Comment for FITS header line
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       1              Input FITS file
CH4       3              Output exposure FITS file
CH4       4              Output intensity FITS file
CH4       6              Printer report
CH4
CH4  Method:
CH4     Read the header of the input FITS file
CH4     Get the header record number and position in the header of
CH4     the BUNIT keyword
CH4     Call WRTLIN to write BUNIT, BSCALE and BZERO in exposure header
CH4     Find the end of the output header
CH4     Call WRTLIN to write the new exposure header lines
CH4     Write the updated exposure header to the exposure FITS file
CH4     Update the BUNIT, BSCALE and BZERO for the intensity map using
CH4     the exposure header buffer
CH4     Write the updated intensity header to the exposure FITS file
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
CH4        Else if (the grid  polar) then
CH4           For (g=1 to the number of groups) do
CH4              Store each exposure parameter in the output record
CH4              Store each intensity parameter in the output record
CH4              If (the output records are full) write the records
CH4              For (p=1 to the number of elements in the group) do
CH4                 Compute the i and j index for group element p
CH4                 Unscale and store the exposure bin data in record
CH4                 Unscale and store the intensity bin data in record
CH4                 If (the output record is full) then
CH4                    Write the exposure record to the FITS file
CH4                    Write the intensity record to the FITS file
CH4                 endif
CH4              end for
CH4           end for
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
CH4  End WRTFTS
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Changed ! comments to C comments.
CH5				Name of counts file passed as argument.
CH5				Form names of exposure and intensity files
CH5				and fopen them.
CH5				Commented out AECON; not required on the SUN.
CH5				7/12/91: Merged changes of version 1.03 on IBM:
CH5     			Re-wrote the part of the program
CH5                             that reads and writes the header so
CH5                             that the entire counts file header
CH5                             doesn't need to be saved in an
CH5                             array which put a limit on the it's size.
CH5				Blanked out the buffer before refilling.
CH5
CH5 $Log: wrtfts.f,v $
CH5 Revision 1.5  2013/05/21 19:08:25  irby
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
CH5 Revision 1.3  2002/12/26 17:42:50  irby
CH5 Fix read/write statements (for f90 compatibility): add/remove commas and
CH5 fix lines over 72 chars.
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
c Version for Linux  2001/08/08  D.L.Bertsch
c The main change is in the calls to fread, fwrite, fopen, and fclose
c and in the inversion of bytes to maintain the same format as Unix
c
c Revision 2.10  2000/12/18  20:52:13  dlb
c Added another decimal point to the format that writes the efficiency.
c
c Revision 2.9  1999/03/08  21:35:02  lmm
c more Y2K date format changes
c
c Revision 2.8  1998/02/24  18:50:00  wang
c  Fix the y2k problems.
c
c Revision 2.7  1995/09/28  20:47:28  programs
c Modified the HISTORY REPROCESS statement to revision_2, September 1995. dlb
c
c Revision 2.6  1995/09/26  20:37:54  nancy
c Included call to generate weighted scaling factors to deal with wide
c energy ranges and non-standard energy bins when the scale factors are
c a function of energy.
c
c Revision 2.5  1995/01/20  20:59:04  albert
c Wrote the calibration table index in the exposure and intensity map headers.
c
c Revision 2.4  1992/10/14  16:30:45  albert
c Modified to write the sensitivity scaling factors in the output maps headers
c
c Revision 2.3  1992/04/01  22:03:23  albert
c Used variable dimension arrays read in as subrouitne parameters. Removed
c special processing for galactic disk maps since no longer needed with
c variable arrays. Used the new set of FITS keywords. Conditionally
c wrote the exposure map depending on whether it already exists or not.
c
c Revision 2.2  1991/11/27  17:33:31  esp
c Minor modification to accommodate new longer file names.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine WRTFTS(output_dir,convert,counts,expbin,intbin)

Cesp  ! Name of the counts file !
      character*(*)  counts, output_dir

Cae   real        exgbin(594,66,10),ingbin(594,66,10)
      real	  expbin(naxis1,naxis2,naxis3)
      real	  intbin(naxis1,naxis2,naxis3)
Cdlb
      real        wtdfac(10)

      integer     mm,dd,yy,convert
      integer     inx,i,j,k,m,n,g,ipos,opos,length,idatt,ifilt,iret

Cdlb  integer     inx,i,j,k,m,n,g,ipos,opos,length,idatt,ifilt
      integer     ibunit,ibscal,ibzero,iblank,iplus,p,h,cdate(3)
      integer*2   expbuf(1440),intbuf(1440)
      character   keywrd*8,cmt*47,string*10,strng2*8,strng3*8,line*80
      logical	  stopwt

      integer     status, fwrite, fopen,  fclose, fread

      equivalence (buffer(1),expbuf(1)) , (buffer(2),intbuf(1))
Cae   equivalence (expbin(1,1,1),exgbin(1,1,1))
Cae   equivalence (intbin(1,1,1),ingbin(1,1,1))

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'

Cesp  ! Names of exposure and intensity output files !
      character(80)	exposure, intensity

      data ibunit/0/, ibscal/0/, ibzero/0/, iblank/0/, opos/1/
      data idatt/0/,ifilt/0/,iplus/80/,strng2/'begin'/,strng3/'begin'/

      character(80)	id, calname
c      character(5)       frmt, formt

      save

      common	/id/id
      id = '$Id: wrtfts.f,v 1.5 2013/05/21 19:08:25 irby Exp $'

Cdlb  ==>  Added call to WTDFACTR to generate spectral-weighted
c     scale factors to written in the header.  This is important
c     for wide energy bands.  For standard bins the weighted 
c     factors are the same as the standard factors.

      iret = 0
      call wtdfactr(wtdfac,iret)

Cesp  ! Form the name of the exposure and intensity files and open it !
      exposure = counts
c      i = len_trim(exposure) - 17
c      write(*,*),'1 exposure: ', exposure, ' i: ', i
c      exposure(i:i+5) = 'exposr'
c      write(*,*),'2 exposure: ', exposure

      exposure = 'exposr' // exposure(10:len_trim(exposure))
      write(*,*) '2a exposure: ', exposure

      intensity = counts
c      intensity(i:i+5) = 'intens'
      intensity = 'intens' // intensity(10:len_trim(intensity))

c     if (.not.usexpf) call fopen(2, 3, exposure, 0, 'FB', 2880, 2880)
c     call fopen(2, 4, intensity, 0, 'FB', 2880, 2880)
c     change for Linux compiler

      ind = len_trim(output_dir)
      write(*,*) 'ind: ', ind
      if (.not.usexpf) then
	 exposure = output_dir(1:ind) // exposure
         write(*,*) '3 exposure: ', exposure
         status = fopen(2, 3, exposure, 0, 'FB', 2880, 2880)
      endif

      intensity = output_dir(1:ind) // intensity
      write(*,*) '3 intensity: ', intensity
      status = fopen(2, 4, intensity, 0, 'FB', 2880, 2880)
      
      buffer(2) = ' '

C---> Read the next record header of the input event counts map
      write(6,'(///,'' Exposure file header:'')')
 10   continue
c     Call FREAD(buffer(1),1,length,*2000,*3000)
c     change for Linux compiler
      status = FREAD(buffer(1),1,length)
      if( status.eq.1 ) go to 2000
      if( status.eq.2 ) go to 3000
Cesp  ! Not needed on the sun since it is native ascii !
Cesp  Call AECON(1,buffer(1),2880,ierr)
Cesp  if (ierr.ne.0)write(6,*)'READFT: number ASCII-EBCDIC errors=',ierr
      ipos = 1
      keywrd = ' '

C---> Get the next line of the current input header record
 20   continue
      line = buffer(1)(ipos:ipos+79)
      keywrd = line(1:8)

C---> Replace the appropriate values in the exposure file header

C---> use current date to determine if observation date and creation date
C---> should be in new Y2K format
      call GIDATE(cdate)
 30   continue
      if (keywrd.eq.'BITPIX') then
         cmt = 'Number of bits per pixel (2-byte integers)'
         call WRTLIN(line,'BITPIX  ',' ','I2   ',' ',16,0.0,cmt)
      else if (keywrd.eq.'BUNIT') then
         cmt = 'Units of primary array'
         call WRTLIN(line,'BUNIT   ',' ','A8   ','EXPOSURE',0,0.0,cmt)
         ibunit = opos
      else if (keywrd.eq.'BSCALE') then
         cmt = 'Data scaling'
         call WRTLIN(line,'BSCALE  ',' ','E13.7',' ',0,bscale(2),cmt)
         ibscal = opos
      else if (keywrd.eq.'BZERO') then
         cmt = 'Data offset'
         call WRTLIN(line,'BZERO   ',' ','E13.7',' ',0,bzero(2),cmt)
         ibzero = opos
      else if (keywrd.eq.'DATE-OBS') then
         if (line(14:14) .eq. '/') then
            read(line(12:30),'(3(i2,1x))') dd,mm,yy
Clmm        Convert year to 4 digits
            if (yy .gt. 65) then 
               yy=1900+yy
            else 
               yy=2000+yy
            endif 
         else
            read(line(12:30),'(i4,1x,i2,1x,i2,1x)') yy,mm,dd
         endif
         if (cdate(3) .le. 1998) then
            write(string,'(i2.2,a1,i2.2,a1,i2.2)') dd,'/',mm,
     &           '/',mod(yy,100)
            cmt = 'Current date in dd/mm/yy'
            call WRTLIN(line,'DATE-OBS',' ','A8   ',string,0,0.0,cmt)
         else
            write(string,'(i4.4,a1,i2.2,a1,i2.2)') yy,'-',mm,
     &           '-',dd
            cmt = 'Data start date in yyyy-mm-dd'
            call WRTLIN(line,'DATE-OBS',' ','A10   ',string,0,0.0,cmt)
         endif
      else if (keywrd.eq.'DATE-END') then
         if (line(14:14) .eq. '/') then
            read(line(12:30),'(3(i2,1x))') dd,mm,yy
Clmm        Convert year to 4 digits
            if (yy .gt. 65) then 
               yy=1900+yy
            else 
               yy=2000+yy
            endif 
         else
            read(line(12:30),'(i4,1x,i2,1x,i2,1x)') yy,mm,dd
         endif
         if (cdate(3) .le. 1998) then
            write(string,'(i2.2,a1,i2.2,a1,i2.2)') dd,'/',mm,
     &           '/',mod(yy,100)
            cmt = 'Current date in dd/mm/yy'
            call WRTLIN(line,'DATE-END',' ','A8   ',string,0,0.0,cmt)
         else
            write(string,'(i4.4,a1,i2.2,a1,i2.2)') yy,'-',mm,
     &           '-',dd
            cmt = 'Data end date in yyyy-mm-dd'
            call WRTLIN(line,'DATE-END',' ','A10   ',string,0,0.0,cmt)
         endif
      else if (keywrd.eq.'DATE') then
Clmm         call IDATE(cdate)
         if (cdate(3) .le. 1998) then
            write(string,'(i2.2,a1,i2.2,a1,i2.2)') cdate(1),'/',
     *           cdate(2),'/',mod(cdate(3),100)
            cmt = 'Current date in dd/mm/yy'
            call WRTLIN(line,'DATE    ',' ','A8   ',string,0,0.0,cmt)
            idatt = opos
         else
            write(string,'(i4.4,a1,i2.2,a1,i2.2)') cdate(3),'-',
     *           cdate(2),'-',cdate(1)
            cmt = 'Current date in yyyy-mm-dd'
            call WRTLIN(line,'DATE    ',' ','A10   ',string,0,0.0,cmt)
            idatt = opos
         endif
      else if (keywrd.eq.'FILETYPE') then
         cmt = 'Data type in the file'
	 call WRTLIN(line,'FILETYPE',' ','A16  ','EGRET_EXPOSR_MAP',
     &        0,0.0,cmt)
	 ifilt = opos
      else if (keywrd.eq.'HISTORY' .and. line(9:16).eq.'REPROCES') then
         cmt = 'Reprocessed.  August 1999'
         call WRTLIN(line,'HISTORY ','REPROCESS ','A10  ','Revision_3',
     &        0,0.0,cmt)
Cae   else if (keywrd.eq.'BLANK') then
Cae      cmt = 'Null value'
Cae      call WRTLIN(line,'BLANK   ',' ','I9   ',' ',blank(2),0,cmt)
Cae      iplus = 80
Cae      iblank = opos

C---> At the end of the header, add some exposure file header values
      else if (keywrd.eq.'END') then
         cmt = 'Wall distance'
         call WRTLIN(line,'HISTORY ','WALL DIS','I3   ',' ',walldi,
     &        0.0,cmt)
         keywrd = 'ACS'
         iplus = 0
      else if (keywrd.eq.'ACS') then
         cmt = 'ACS sensitivity'
         call WRTLIN(line,'HISTORY ','ACS SENS','I1   ',' ',acs,0.0,cmt)
         keywrd = 'TASC'
      else if (keywrd.eq.'TASC') then
         cmt = 'TASC in coincidence'
         call WRTLIN(line,'HISTORY ','TASC COI','I6   ',' ',tascco,
     &        0.0,cmt)
         keywrd = 'CAL1'
      else if (keywrd.eq.'CAL1') then
         cmt = 'Calibration file names'
	 calname = calfil(1) // calstr
	 numchar = index(calname, ' ') - 1
c	 write(frmt, numchar)
c	 formt = 'A' // frmt
c	 call wrtlin(line,'HISTORY ','CAL FIL1',formt,calname,0,0.0,cmt)
         call WRTLIN(line,'HISTORY ','CAL FIL1','A13  ',calfil(1) // 
     &        calstr,0,0.0,cmt)
         keywrd = 'CAL2'
      else if (keywrd.eq.'CAL2') then
         cmt = 'Calibration file names'
	 calname = calfil(2) // calstr
	 numchar = index(calname, ' ') - 1
c	 write(frmt, numchar)
c	 formt = 'A' // frmt
c	 call wrtlin(line,'HISTORY ','CAL FIL2',formt,calname,0,0.0,cmt)
         call WRTLIN(line,'HISTORY ','CAL FIL2','A13  ',calfil(2) // 
     &        calstr,0,0.0,cmt)
         keywrd = 'SPECIN01'
         strng2 = keywrd
         cmt = 'Spectral index'
         p = 1
      else if (keywrd.eq.strng2) then
         cmt = 'Spectral index'
         call WRTLIN(line,'HISTORY ',strng2,'F7.2 ',' ',0,specin(p),cmt)
         if (p .lt. naxis3) then
            p = p + 1
            write(strng2,'(a6,i2.2)') 'SPECIN',p
            keywrd = strng2
         else
            keywrd = 'SCFACT01'
            strng3 = keywrd
            cmt = 'Sensitivity scaling factor'
            p = 1
         end if
      else if (keywrd.eq.strng3) then
         cmt = 'Sensitivity scaling factor'
Cdlb     call WRTLIN(line,'HISTORY ',strng3,'F7.2 ',' ',0,sfact(p),cmt)
         call WRTLIN(line,'HISTORY ',strng3,'F7.3 ',' ',0,wtdfac(p),cmt)

         if (p .lt. naxis3) then
            p = p + 1
            write(strng3,'(a6,i2.2)') 'SCFACT',p
            keywrd = strng3
         else
            keywrd = 'LAST    '
         end if
      else if (keywrd.eq.'LAST    ') then
         keywrd = 'FINISH'
         call WRTLIN(line,'END     ',' ','A1   ',' ',0,0.0,' ')
      end if

C---> Copy the line into the output buffer
      buffer(2)(opos:opos+79) = line
      opos = opos + 80

C---> If the output buffer is full, write the exposure buffer first
      if (opos.ge.2880 .or. keywrd.eq.'FINISH') then
	 stopwt = .false.
         do k=1,2801,80
	    if (.not.stopwt) write(6,'(a80)') buffer(2)(k:k+79)
	    if (buffer(2)(k:k+4).eq.'END ') stopwt = .true.
         end do
Cesp	 ! Not required on the SUN since it is native ascii !
Cesp     call AECON(-1,buffer(2),2880,ierr)
c     if (.not.usexpf) call FWRITE(buffer(2),3,2880)
c     change for Linux compiler
         if (.not.usexpf) status = FWRITE(buffer(2),3,2880)

C------> Modify the header buffer for the intensity file
         if (ibunit .ne. 0) then
            cmt = 'Units of primary array'
            call WRTLIN(line,'BUNIT   ',' ','A9   ','INTENSITY',0,
     &           0.0,cmt)
Cesp	    ! Not required on the SUN since it is native ascii !
Cesp        call AECON(-1,line,80,ierr)
            buffer(2)(ibunit:ibunit+79) = line
            ibunit = 0
         endif
         if (ibscal .ne. 0) then
            cmt = 'Data scaling'
            call WRTLIN(line,'BSCALE  ',' ','E13.7',' ',0,bscale(3),cmt)
Cesp	    ! Not required on the SUN since it is native ascii !
Cesp        call AECON(-1,line,80,ierr)
            buffer(2)(ibscal:ibscal+79) = line
            ibscal = 0
         end if
         if (ibzero .ne. 0) then
            cmt = 'Data offset'
            call WRTLIN(line,'BZERO   ',' ','E13.7',' ',0,bzero(3),cmt)
Cesp	    ! Not required on the SUN since it is native ascii !
Cesp        call AECON(-1,line,80,ierr)
            buffer(2)(ibzero:ibzero+79) = line
            ibzero = 0
         end if
         if (idatt .ne. 0) then
            if (cdate(3) .le. 1998) then
               cmt = 'Current date in dd/mm/yy'
               call WRTLIN(line,'DATE    ',' ','A8   ',string,0,0.0,cmt)
               buffer(2)(idatt:idatt+79) = line
               idatt = 0
            else
               cmt = 'Current date in yyyy-mm-dd'
               call WRTLIN(line,'DATE    ',' ','A10   ',
     *              string,0,0.0,cmt)
               buffer(2)(idatt:idatt+79) = line
               idatt = 0
            endif
         end if
         if (ifilt .ne. 0) then
            cmt = 'Data type in the file'
	    call WRTLIN(line,'FILETYPE',' ','A16  ','EGRET_INTENS_MAP',
     &           0,0.0,cmt)
            buffer(2)(ifilt:ifilt+79) = line
            ifilt = 0
         end if
Cae      if (iblank .ne. 0) then
Cae         cmt = 'NULL VALUE'
Cae         call WRTLIN(line,'BLANK   ',' ','I9   ',' ',blank(3),0,cmt)
Cesp	    ! Not required on the SUN since it is native ascii !
Cesp        call AECON(-1,line,80,ierr)
Cae         buffer(2)(iblank:iblank+79) = line
Cae         iblank = 0
Cae      end if
c     call FWRITE(buffer(2),4,2880)
c     Change for Linux compiler
         status = FWRITE(buffer(2),4,2880)
   	 buffer(2) = ' '
         opos = 1
      end if

C---> Increment the input buffer line
      if (keywrd .ne. 'FINISH') then
         if (iplus .eq. 0) goto 30
         ipos = ipos + iplus
         if (ipos .ge. 2880) goto 10
         goto 20
      end if

      inx = 1

C---> Loop over the energy levels
      do k=1,naxis3
C						! Energy levels

C------> Write the rectangular map data to the file
         if (gridtp.eq.'RECT') then

C---------> Loop over the map bins and write the buffer when full
            do j=1,naxis2
C					   ! Y axis
               do i=1,naxis1
C					   ! X axis
Cae               if (naxis1.le.200) then
                  expbuf(inx) = (expbin(i,j,k)-bzero(2))/bscale(2)
                  intbuf(inx) = (intbin(i,j,k)-bzero(3))/bscale(3)
Cae               else
Cae                  expbuf(inx) = (exgbin(i,j,k)-bzero(2))/bscale(2)
Cae                  intbuf(inx) = (ingbin(i,j,k)-bzero(3))/bscale(3)
Cae               endif
                  inx = inx + 1
                  if (inx.gt.1440) then
c     if (.not.usexpf) call FWRITE(expbuf,3,2880)
c     call FWRITE(intbuf,4,2880)
c     change for Linux compiler
                     if (.not.usexpf) then
                        if (convert .eq. 1)
     *                     call reflect( expbuf, 2, 2880 )
                        status = FWRITE(expbuf,3,2880)
                     endif
                     if (convert .eq. 1)
     *                  call reflect( intbuf, 2, 2880 )
                     status = FWRITE(intbuf,4,2880)
                     inx = 1
                  endif
               end do
            end do

C------> Write the polar map data to the FITS file
         else if (gridtp.eq.'POLA') then
            i = 0

C---------> Loop over the map bins
            do g=1,gcount
C					! Loop over groups
               do m=1,int(ftparm(1,g))
C					! Loop over # elements in group
C---------------> Write the group parameters first
                  if (m.eq.1) then
                     h = 1
                     do p=1,pcount
C					! Loop over # of parameters
                        expbuf(inx) = ftparm(p,g) * h
                        intbuf(inx) = ftparm(p,g) * h
                        inx = inx + 1
                        if (inx.gt.1440) then
c     if (.not.usexpf) call FWRITE(expbuf,3,2880)
c     call FWRITE(intbuf,4,2880)
c     change for Linux compiler
                           if (.not.usexpf) then
                              if (convert .eq. 1)
     *                           call reflect( expbuf, 2, 2880 )
                              status = FWRITE(expbuf,3,2880)
                           endif
                           if (convert .eq. 1)
     *                        call reflect( intbuf, 2, 2880 )
                           status = FWRITE(intbuf,4,2880)
                           
                           inx = 1
                        endif
                        h = 10
                     end do
                     n = 360 / ftparm(5,g)
                  endif

C---------------> Compute the array indexes from the group element
                  if (j.gt.n.or.m.eq.1) then
                     i = i + 1
                     j = 1
                  endif

C---------------> Copy the bin data to the FITS record. write when full
                  expbuf(inx) = (expbin(i,j,k)-bzero(2))/bscale(2)
                  intbuf(inx) = (intbin(i,j,k)-bzero(3))/bscale(3)
                  inx = inx + 1
                  if (inx.gt.1440) then
c     if (.not.usexpf) call FWRITE(expbuf,3,2880)
c     call FWRITE(intbuf,4,2880)
c     change for Linux compiler
                     if (.not.usexpf) then
                        if (convert .eq. 1)
     *                     call reflect( expbuf, 2, 2880 )
                        status = FWRITE(expbuf,3,2880)
                     endif
                     if (convert .eq. 1)
     *                  call reflect( intbuf, 2, 2880 )
                     status = FWRITE(intbuf,4,2880)
                     inx = 1
                  endif

                  j = j + 1
               end do
            end do

C------> Do the Aitoff maps
         else if (gridtp.eq.'AITF') then

C---------> Loop over the latitudes
            do j=1,naxis2

C------------> Write the group parameters for each group
	       h = 1
               do p=1,pcount
                  expbuf(inx) = ftparm(p,j) * h
                  intbuf(inx) = ftparm(p,j) * h
                  inx = inx + 1
                  if (inx.gt.1440) then
c     if (.not.usexpf) call FWRITE(expbuf,3,2880)
c     call FWRITE(intbuf,4,2880)
c     change for Linux compiler
                     if (.not.usexpf) then
                        if (convert .eq. 1)
     *                     call reflect( expbuf, 2, 2880 )
                        status = FWRITE(expbuf,3,2880)
                     endif
                     if (convert .eq. 1)
     *                  call reflect( intbuf, 2, 2880 )
                     status = FWRITE(intbuf,4,2880)
                     inx = 1
                  endif
		  h = 100
               end do

C------------> Loop over the longitudes
               do i=1,naxs12(j)

C---------------> Copy the bin data to the FITS record. write when full
                  expbuf(inx) = (expbin(i,j,k)-bzero(2))/bscale(2)
                  intbuf(inx) = (intbin(i,j,k)-bzero(3))/bscale(3)
                  inx = inx + 1
                  if (inx.gt.1440) then
c     if (.not.usexpf) call FWRITE(expbuf,3,2880)
c     call FWRITE(intbuf,4,2880)
c     change for Linux compiler
                     if (.not.usexpf) then
                        if (convert .eq. 1)
     *                     call reflect( expbuf, 2, 2880 )
                        status = FWRITE(expbuf,3,2880)
                     endif
                     if (convert .eq. 1)
     *                  call reflect( intbuf, 2, 2880 )
                     status = FWRITE(intbuf,4,2880)
                     inx = 1
                  endif

               end do
            end do

         end if
C     ! for grid type

      end do
C     ! for energy levels

C---> Write the last record if necessary
c     if (inx.gt.1 .and. .not.usexpf) call FWRITE(expbuf,3,2880)
c     if (inx.gt.1) call FWRITE(intbuf,4,2880)
c     change for Linux compiler
      if (inx.gt.1 .and. .not.usexpf) then
         if (convert .eq. 1)
     *      call reflect( expbuf, 2, 2880 )
         status = FWRITE(expbuf,3,2880)
      endif
      if (inx.gt.1) then
         if (convert .eq. 1)
     *      call reflect( intbuf, 2, 2880 )
         status = FWRITE(intbuf,4,2880)
      endif


Cesp  ! Close the files opened with fopen !
      if (.not.usexpf) status = fclose(3)
      status = fclose(4)
      
      write(*,*) 'return from wrtfts'
      return

 2000 write(6,*) 'WRTFTS: premature end of file in the FITS file'
      retcod = 8
      return
 3000 write(6,*) 'WRTFTS: I/O error in reading the FITS file'
      retcod = 12
      return
CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(WRTFTS) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
