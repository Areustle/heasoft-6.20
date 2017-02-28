CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(FNDFIL) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  FNDFIL
CH1
CH1  Version: 1.01             Revision Date: 03/26/91
CH1  Version: 2.00             Date: 06/20/91
CH1  $Id: fndfil.f,v 1.6 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Find the exposure history file to use from the data time
CH1            range and the timeline file.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call FNDFIL(exphst)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2Cesp exphst      Ch*24  I/O  Name of the exposure history file
CH2     exphst      Ch*(*) I/O  Name of the exposure history file
CH2
CH2  Called by: INTMAP
CH2
CH2  Calls:
CH2   CONVRT: EGRET Utility Routine to convert TJD/MSD into single R*8
CH2   GRDTJD: EGRET Utility Routine to convert from gregorian to TJD/MSD
CH2   TJDGRD: EGRET Utility Routine to convert from TJD/MSD to gregorian
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
CH3  vstime     R*8       0.0     Viewing period start time
CH3  vetime     R*8       0.0     Viewing period end time
CH3  viewnm     I*4        -      Viewing period number
CH3  found      L*4        F      Determines if viewing period found
CH3  linbuf     Ch*80      -      Line buffer read from timeline file
CH3  keywrd     Ch*8       -      Keyword from line buffer
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       2     tunit    Timeline file
CH4       8     eunit    Exposure history file
CH4       6              printer report
CH4
CH4  Method:
CH4   Initialize variables
CH4   While (found flag = false & no EOF) do
CH4      Read the next timeline file record
CH4      If (the keyword = 'START TIME') save the viewing start time
CH4      If (the keyword = 'END TIME') save the viewing end time
CH4      If (the end time was found) then
CH4         If (data time range is within viewing time range) found = T
CH4         Else clear the end time
CH4      End if
CH4   End while
CH4   If (end of file) exit with error message
CH4   Generate the name of the exposure history file from the period #
CH4  End FNDFIL
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5     1.01    A.Etienne 03/26/91   Allowed for blank dates and times
CH5                                  in timeline file. In that case,
CH5                                  the previous date/time is used.
CH5
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Changed ! comments to C comments
CH5				Get path name of time line file and open it.
CH5				Exposure history file has path name in it and
CH5				hence of variable length.
CH5				Merged changes from version 1.03 on IBM.
CH5				Called routine UPCASE to read the
CH5                             timeline file keywords both in
CH5                             upper and lower case. Allowed for
CH5                             'END VIEWING' and 'STOP VIEWING'.
CH5     2.01    S. Bansal       Read misc_dir from intmap.par in intmap.f and
CH5                             pass it to this function in parameter list.
CH5                             Comment out the code to get misc_dir env var.
CH5 $Log: fndfil.f,v $
CH5 Revision 1.6  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.5  2006/04/17 20:54:27  irby
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
CH5 Revision 1.4  2005/08/26 19:52:02  irby
CH5 gfortran/g95 does not allow equivalence statements containing references
CH5 to substrings, e.g. "equivalence (linbuf(32:39),keywrd)", so these have
CH5 been reworked.
CH5
CH5 Revision 1.3  2002/12/26 17:16:31  irby
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
CH5 Revision 1.1  2002/04/16 20:24:02  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.2  1998/02/24  18:49:38  wang
c  Fix the y2k problems.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CH5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine FNDFIL(misc_dir, exphst)

c     INCLUDE 'cnfrep.copy'

      real*8    CONVRT,vstime,vetime,curtim
      integer   viewnm,tjd,msd,yy,mm,dd,hr,mn,sc,mil,iret
      logical   found,newtim

Cesp  ! Exposure history file now includes a path name !
Cesp  character linbuf*80,keywrd*8,exphst*24
      character exphst*(*)
      character(8) keywrd, upcase
      character(80) linbuf

Cesp  ! Time line file name !
      integer		i

      character(80)  timeline
      character*(*) misc_dir

      include '../INTMAP_COMMON/global.cmn'

      save

      data found/.false./,vstime/0.0/,vetime/0.0/,newtim/.false./

      character(80)	id
      common	/id/id
      id = '$Id: fndfil.f,v 1.6 2013/05/21 19:08:24 irby Exp $'

C---> Initialize unit numbers
      tunit = 2
      eunit = 8
      write(6,'(''1'')')

Cesp  ! First open the timeline file !

Csb   Read timeline (misc_dir) from intmap.par in intmap.f
c     call getenv('MISC_DIR', timeline)

      timeline = misc_dir(1:len_trim(misc_dir)) // '/timeline'


      open(unit=tunit, file=timeline,iostat=ios)

C---> Read each timeline file record and search for the start time
      do while (.not.found)
         Read(tunit,'(a80)',end=100) linbuf

Cesp     ! F77 v1.2 does not implement ! comments
Cesp     if (linbuf(1:1) .eq. '*') goto 10         !Skip comment records
Cesp	 !Skip comment records
         if (linbuf(1:1) .eq. '*') goto 10

C------> Get the current time (if there is one on this line)
         if (linbuf(8:15) .ne.' ') then
            read(linbuf(8:15),'(3(i2,1x))') mm,dd,yy
            newtim = .true.
         end if
         if (linbuf(18:29).ne.' ') then
            read(linbuf(18:29),'(3(i2,1x),i3)') hr,mn,sc,mil
            newtim = .true.
         end if
         if (newtim) then
            if (yy .le. 65) yy = yy + 100
            call GRDTJD(tjd,msd,yy+1900,mm,dd,hr,mn,sc,mil,iret)
            if (iret.ne.0) goto 200
            curtim = CONVRT(tjd,msd)
            newtim = .false.
         end if
	 keywrd = UPCASE(linbuf(32:39))

C------> Check if the line has a start time
         if (keywrd.eq.'START VI') then
            read(linbuf(2:5),'(i4)') viewnm
            vstime = curtim
c           write(6,2000) 'start',mm,dd,yy,hr,mn,sc,mil,tjd,msd
         end if

C------> Check if the line has an end time
         if (keywrd.eq.'STOP VIE' .or. keywrd .eq. 'END VIEW') then
            vetime = curtim
c           write(6,2000) 'end  ',mm,dd,yy,hr,mn,sc,mil,tjd,msd
         end if

C------> Check if the viewing time range is within the data time range
         if (vetime.ne.0.0) then
            if (strtim.ge.vstime.and.endtim.le.vetime) then
               found = .true.
            else
               vetime = 0.0
            end if
         end if

10       continue
      end do

C---> Generate the name and open the exposure history file
Cesp  ! Exphst is of variable length on the SUN. !
Cesp  write(exphst(18:21),'(i4.4)') viewnm

      i = len_trim(exphst)
      write(exphst(i-3:i),'(i4.4)') viewnm

      write(6,*) 'Exposure history file found = ',exphst
Cesp  open(unit=eunit,err=300,status='old',file='/'//exphst)
      open(unit=eunit,err=300,status='old',file=exphst)

      return

C---> Data time range not found in timeline file
100   continue
      write(6,*) 'FNDFIL: Data time range not matched in timeline file'
      retcod = 8
      return

C---> Invalid time
200   continue
      write(6,*) 'FNDFIL: invalid time found in timeline file'
      write(6,*) linbuf
      write(6,*) mm,dd,yy,hr,mn,sc,mil,tjd,msd
      retcod = 8
      return

C---> Exposure history file cannot be opened
300   continue
      write(6,*) 'FNDFIL: Can''t open exposure history file'
      write(6,*) 'File name = ',exphst
      retcod = 8
      return

1000  format(3(i2,1x),3(1x,i2),1x,i3)
c2000  format(' Viewing period ',a5,' time = ',2(i2.2,'/'),i2.2,1x,
c     &       2(i2.2,':'),i2.2,'.',i3.3,3x,'(',i5,',',i8,')')
CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(FNDFIL) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
