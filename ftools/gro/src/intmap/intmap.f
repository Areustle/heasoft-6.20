CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(INTMAP) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  INTMAP
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: intmap.f,v 1.4 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Generate the intensity and exposure maps.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  N.A.
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  Called by:  None
CH2
CH2  Calls:
CH2   ZTIME : System routine to get current time
CH2   READFT: Read a FITS format file
CH2   FNDFIL: Find the exposure history file to use
CH2   EXPOSR: Generate the exposure map
CH2   INTENS: Generate the intensity map
CH2   WRTFTS: Write the FITS format files
CH2
CH3  COMMON Use:
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
CH3  today      Ch*16      -      Today's date
CH3  filnam     Ch*28      -      Name of the counts map file
CH3  exphst     Ch*24      -      Name of the exposure history file
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       5              User input Namelist
CH4       6              Printer report
CH4
CH4  Method:
CH4    Write the program title, version number and date
CH4    Read and echo the user input
CH4    Call READFT to read the input FITS file
CH4    Call FNDFIL to find the exposure history file
CH4    Call EXPOSR to generate the exposure map
CH4    Call INTENS to generate the intensity map
CH4    Call WRTFTS to write the FITS files generated
CH4  End INTMAP
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Ztime changed to fdate.
CH5				Today changed to character(24).
CH5				fname and exphst changed to character(80).
CH5				Opened name list file before reading.
CH5				Filename passed to READFT and WRTFTS.
CH5
CH5 $Log: intmap.f,v $
CH5 Revision 1.4  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.3  2002/12/26 17:42:49  irby
CH5 Fix read/write statements (for f90 compatibility): add/remove commas and
CH5 fix lines over 72 chars.
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:03  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.4  1992/10/14  16:09:07  albert
c Made the solid angle variable dependent on the energy level so as to
c include the sensitivity scaling factor. Included the scaling factors in
c the namelist. Cosmetic changes on the title and user input echo.
c
c Revision 2.3  1992/04/08  15:16:51  albert
c Made the polar and azimuth angles arrays to be variable dimensioned like
c the bin arrays and passed them to the exposr routine.
c
c Revision 2.2  1992/04/01  21:44:28  albert
c Used variable dimension arrays by declaring them locally and passing them
c as parameters to the various subroutines. Allowed bypassing the computation
c of the exposure file if it already exists and if requested by the user.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine INTMAP

c     INCLUDE 'cnfrep.copy'

      integer   i

Cesp  ! Today changed to character(24)	!
Cesp  ! filnam and exphst changed to character(80)	!
Cesp  character today*16,filnam*28,exphst*24


      character(80)  filnam, exphst, data_dir, misc_dir, calib_dir
      character(80)  cal_bin_dir, output_dir
      character today*24, arch*20
      include '../INTMAP_COMMON/global.cmn'

C---> Bin data arrays
      real    expbin(maxbin),intbin(maxbin)
      real    polang(259200),azmang(259200)
      real    solidn(720,10)
      integer cntbin(maxbin), convert, status

c     namelist /userin/specin,filnam,exphst,usexpf,sfact 

      character(80)	id
      common	/id/id

      id = '$Id: intmap.f,v 1.4 2013/05/21 19:08:24 irby Exp $'


C---> Initialize program return code
      retcod = 0
      status = 0

C---> Print the program title
Cesp  ! Ztime (IBM) changed to fdate(f77)
Cesp  call ZTIME(today,2)

      call fdate(today)

Cesp  ! today is now a 24 character string !
Cesp  write(6,'(116x,a16)') today

      write(6,'(26x,'' I N T M A P   P R O G R A M'',//,28x,a)') today

Cesp  write(6,'(//,63x,''10/26/90'',/,64x,''V 1.00'')')

      write(6,'(//,14x,a)') id

      call system("uname -s > archfile")
      open (10, file="archfile")
      read(10, *) arch
      close(10)
      call system ("rm -f archfile")

      convert = 0
      if (index(arch, 'Sun') .eq. 0) then
         if (index(arch, 'OSF') .eq. 0) 
     *       convert = 1
      endif

csb   Read input parameter file intmap.par
      call read_input(data_dir, filnam, exphst, usexpf, specin, 
     *		      sfact, misc_dir, calib_dir, output_dir, 
     *		      cal_bin_dir, status)
      if (status .ne. 0) return

C---> Read and echo the user input

Cesp  ! First open the namelist file !
c      open(5, file='namelist.int')

c      read(5,userin)

      write(6,'(///,''USER INPUT:'',/,''-----------'')')

Cesp  ! filnam is now an 80 character variable string !
Cesp  write(6,'('' Input Event File Name = '',a28)') filnam
      write(6,'(''Input Event File Name = '',/,a)') filnam
      write(6,'(''Input exposure File Name = '',/,a)') exphst

      if (usexpf) write(6,*) 'The old exposure file will be reused'

      write(6,'(/,''Spectral Indexes ='',10f6.2)')(specin(i),i=1,10)
      write(6,'(''Scaling Factors  ='',10f6.2)')(sfact(i),i=1,10)

C---> Read the input FITS file of event counts data
Cesp  ! on the sun readft needs to open the file; hence file name passed !
Cesp  call READFT

      call READFT(data_dir,filnam,convert,cntbin,expbin)

      if (retcod.ne.0) goto 9000

C---> If the exposure file does not exist then generate it
      if (.not.usexpf) then

C------> Find the exposure history file
         call FNDFIL(misc_dir, exphst)
         if (retcod.ne.0) goto 9000

C------> Generate the exposure file
         call EXPOSR(data_dir,calib_dir,cal_bin_dir,expbin,intbin,
     *        polang,azmang,solidn)
         if (retcod.ne.0) goto 9000

      endif

C---> Generate the intensity file
      call INTENS(cntbin,expbin,intbin)
      if (retcod.ne.0) goto 9000

C---> Write the output FITS files to disk
Cesp  ! on the sun wrtfts needs to open the file; hence file name passed !
Cesp  call WRTFTS
      call WRTFTS(output_dir,convert,filnam,expbin,intbin)

      if (retcod.ne.0) goto 9000
      
      write(6,'(//,'' INTMAP program terminated'')')
      return

9000  write(6,'(//,'' INTMAP program terminated due to errors'')')
      return
c      if (retcod.eq.4) stop 4
c      stop 8

CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(INTMAP) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
