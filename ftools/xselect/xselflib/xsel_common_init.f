c Initialization code which is common between the different xselects

      SUBROUTINE xsel_common_init(prefix_str, prefixlen)

      IMPLICIT NONE

      CHARACTER(255) prefix_str
      INTEGER prefixlen

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
      INCLUDE 'xselplt.inc'

c ---------------------------------------------
c DECLARATIONS
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1
c General reusable integers for lengths of strings 
      integer len1, len2, len3
      
c ---------------------------------------------
c To test for presence of saved session
      logical SAVED
c ---------------------------------------------
c True if the batchmode parameter is set
C      logical QBATCH
c LENACT, and i
      integer LENACT
      integer i
c OLD is for setting values if you use an old save file
      integer old

      INTEGER XSELECT_APE_INIT

c ---------------------------------------------
c Big block of initializations. Cannot do this through
c data statements any more because these variables are
c all in common blocks
c ---------------------------------------------
c Fill logicals and filenames with their startup values.

      READ      = .FALSE.
      SPEC      = .FALSE.
      CURV      = .FALSE.
      SELCT     = .FALSE.
      MERGED    = .FALSE.
      MANY      = .FALSE.
      MADE      = .FALSE.
      WORK      = .FALSE.
      MERGTI    = .FALSE.
      FILTER    = .FALSE.
      USEGTI    = .FALSE.
      MERGHK    = .FALSE.
      USEHK     = .FALSE.
      EXPAND    = .FALSE.
      HKSEL     = .FALSE.
      MANYHK    = .FALSE.
      HKREAD    = .FALSE.
      WORKHK    = .FALSE.
      LOADED    = .FALSE.
      IMAGE     = .FALSE.
      CLEAN     = .FALSE.
      CLEANI    = .FALSE.
      ECHO      = .FALSE.
      FITTFL    = .FALSE.
      ASCTFL    = .FALSE.
      XWNTFL    = .FALSE.
      REGION    = .FALSE.
      FFTFL     = .FALSE.
      UNBIN     = .FALSE.
      EVTSAV    = .FALSE.
      CLNSAV    = .FALSE.
      HKCURV    = .FALSE.
      FFCURV    = .FALSE.
      USEQDP    = .FALSE.
      SMOOTH    = .FALSE.
      SAVSMOOTH = .FALSE.
      FAST      = .FALSE.
      FAINT     = .FALSE.
      INTENS    = .FALSE.
      WTMAPB    = .FALSE. 
      SHOWOC    = .TRUE. 
      VIMAGE    = .TRUE. 
      VSPEC     = .TRUE. 
      VREGION   = .TRUE. 
      VPHACUT   = .TRUE. 
      VGISCLEAN = .FALSE. 
      HAVEMKF   = .FALSE. 
      BINOUT    = .FALSE.
      BININ     = .FALSE.
      DETFL     = .FALSE.
c ---------------------------------------------
c Number of files read in
      nfiles = 0
c Number of HK files read in
      nhkfil = 0
c Number of hk parameters read in
      npar = 0
c Number of ff parameters read in
      nffpar = 0
c Set the number of entered filters to 0
      numreg = 0
      numdet = 0
      numasc = 0
      numgti = 0
      numcti = 0
      numhnd = 0
c Filter file name
      ffilin = 'def'
c Number of filter files entered
      nmkf = 0     
c set the data dir and mkfdir to empty values
      datdir = 'NONE'
      mkfdir = 'NONE'
      mkfdnm = 'NONE'
c Set the plot device to NONE
      plotdv = 'NONE'
c This flags that the list of drivers has not yet been loaded
      devlst(1) = 'NOT_LISTED'
c This flags that the list of mkf parameters have not been loaded
      mkfpar(1) = 'NOT_LISTED'
c Set instrument to none at start
      instru = 'NONE'
c set the datamode to it's empty value
      datamode = 'NONE'
c There is no dfe file yet:
      usrdfe = '-'
c Set the default pagewidth size
      pgwth = 128
c Set the selection, HK & catalogue selection to NONE:
      hkstr  = 'NONE'
      catflt = 'NONE'
      choflt = 'NONE'
      catsel = 'NONE'
      do i=1,MXHIST
         strsel(i) = 'NONE'
         ffstr(i) = 'NONE'
      enddo
      nstrsel = 0
      nffstr  = 0
c Set number of phase and intensity intervals to 0
      nphase = 0
      numint = 0
c Set the grade filter to NONE
      gfilter = 'NONE'
c Set the column filter to a blank string
      colfilter = ' '

c Set DEFAULT MISSION to ASCA
      keymis = 'ASCA'
c Set DEFAULT SUBMIS to NONE
      submis = 'NONE'
c Initialize the status parameter:
      status = 0
c Initialize the timdel keyword(-ve means it has not been found yet):
      timedel = -1.0d0
c Initialize the catalogue name:
      catnam = 'NONE'

c Set the evtnum and gtinum to the ASCA defaults:
      evtnum = '1  '
      gtinum = '2  '
      
c Negative values for phalcut and phahcut signal not yet set:
      phalcut = -20
      phahcut = -20
c set the phamin and max
      phamin = 100000
      phamax = 0
c The default pha binsize should be 1
      phabin =  1 
c The default x binning factor for the image
      xbinf = 1
c Set the offset and size factors:
      xch = 0
      ych = 0
      xcf = 0
      ycf = 0
      sizeh = -1
      sizef = -1
c The day2sec constant:
      day2sec =  86400D0 


c ############################################################
c NOTE: PLACES TO FIDDLE WITH TO GET OTHER MISSIONS TO WORK:
c Any mission using event files that requires no special ftools
c can be added by modifying the xsel14.mdb file. The best way
c to start is to copy the entries for an existing mission.
c If the mission requires special ftools then look in the file
c xsel_mission.f file for the current mission-specific routines
c (ASCA and ROSAT) and see whether any of these can be adapted.
c ############################################################
 

      do i=1,MXNSEL
         CTINDX(i) = 0
         HNDNDX(i) = 0
         INTNDX(i) = 0
      enddo

c ------------------------------
c Now set up version number, and so on...

      include 'xsel_ver.inc'

c ------------------------------

c  Initialize the filename arrays:
      do i=1,MAXFIL
         filenm(i) = ' '
         hkflnm(i) = ' '
         gtifnm(i) = ' '
      enddo

c
c
c ---------------------------------------------
c PROGRAM STARTS IN EARNEST
c ---------------------------------------------
c
c

c Initialize XPARSE
      CALL XPARSE(' ')

c Set up the chattiness for XWRITE
      CALL XCHATY(6,10)

      status = XSELECT_APE_INIT()
      if (status .ne. 0) call XSL_EXIT(status)

c First, load the par file:
      len1 = LENACT(xslnam)
      len2 = LENACT(xsldsk)
      len3 = lenact(xsldir)
      CALL TBLDSTAND(xslnam(:len1),xsldsk(:len2),xsldir(:len3),
     &               ierr)

c type the startup banner
      
      call XWRITE(' ',5)
      write(str1,10)xslver(1:1),xslver(2:4)
 10   format(25x,'**  XSELECT V',a1,'.',a3,' ** ')
      call XWRITE(str1,5)
      call XWRITE(' ',5)

c Now prompt for the prefix, and load the file names:

      call XSL_LOAD_NAMES(0)

      call XSL_RESTORE(SAVED,old)

c Check whether this process has been told that it is running in
c background

      call uclgsb('batchmode',QBATCH,status)

c Locate ref data dir and the leapsecond file 
      
      call UCLGST('ref_data_dir',refdir,status)
      call XSL_EXPAND_DIR(refdir,status)

      if ( status.ne.0 ) then
         call XWRITE
     &        ('Warning: could not find ref data directory at',5)
         str1 = '   '//refdir(:min(lenact(refdir),len(str1)-3))
         call XWRITE(str1,5)
         call XWRITE('The commands FILTER TIME UT, GISCLEAN and'//
     &        ' FAINT bright=no require this.',5)
         call XWRITE('It can be found at the same place you got '//
     &        'the FTools tar file.',5)
         call XWRITE('If you stored it in a different place than'//
     &        ' the FTools area, quit Xselect, set the path with:',5)
         str1 = '    pset xsel'//' ref_data_dir=your_path'
         call XWRITE(str1,5)
         call XWRITE('and then restart Xselect.',5)
         refdir = 'NONE'
         status = 0
      endif

c If there is no save file, the mission is set for the value
c given in the default_mission parameter

      IF(SAVED) then

C Load all the generic Mission parameters:

         call XSL_SET(3)
         IF(old.eq.1)THEN
            call XWRITE('Save files before .96 don''t store '//
     &           'coordinates',5)
            call XWRITE('Setting them to default values',5)
            call XSL_SET(2)
         ENDIF

C Now set the prompt:

         prompt = prefix(:LENACT(prefix))
         IF(keymis.ne.'NONE') THEN
            prompt = prompt(:LENACT(prompt))//':' 
     &           //keymis(:LENACT(keymis))
         ENDIF
         IF(instru.ne.'NONE') then
            prompt = prompt(:LENACT(prompt))//'-'//
     &           instru(:LENACT(instru))
         ENDIF          
         IF(datamode.ne.'NONE') then
            prompt = prompt(:LENACT(prompt))//'-'//
     &           datamode(:LENACT(datamode))
         ENDIF          
         prompt = prompt(:LENACT(prompt))//' >'
         SAVED = .FALSE.

      ELSE

C Reset the values stored in the par file.

         call UCLGSR('stored_binsize',binsiz,status)

c Don't reset binsize to 0 by default:

         IF(binsiz.eq.0) THEN
            binsiz = 16.0
            call XWRITE(
     &           'Warning - resetting binsize from 0.0 to 16.0',5)
         ENDIF
         status = 0
         call UCLGSI('stored_pharebin',phabin,status)
         status = 0
         call UCLGST('stored_devicetype',str1,status)
         plotdv = str1(:min(lenact(str1),len(plotdv)))
         call XSL_SET(2)

      ENDIF

c If plotdv has not been set by either of these, then 
c look for the PGPLOT_TYPE environment variable, and if present,
c set plotdv:

      IF(plotdv.eq.'NONE'.or. plotdv.eq.'/NONE') THEN
         call TRLOG('PGPLOT_TYPE',11,str1,len1)
         if(len1.ne.0) then
            write(*,*) 'Setting plot device to ',str1(:len1)
            plotdv = str1(:len1)
         endif
      ENDIF

c First set up the prompt

      prompt = prefix(:LENACT(prefix))
      IF(keymis.ne.'NONE') THEN
         prompt = prompt(:LENACT(prompt))//':'
     &        //keymis(:LENACT(keymis))
      ENDIF
      IF(instru.ne.'NONE') then
         prompt = prompt(:LENACT(prompt))//'-'
     &           //instru(:LENACT(instru))
      ENDIF
      IF(datamode.ne.'NONE') then
         prompt = prompt(:LENACT(prompt))//'-'
     &           //datamode(:LENACT(datamode))
      ENDIF

      prompt = prompt(:LENACT(prompt))//' >'
      prefix_str = prompt(:lenact(prompt))
      prefixlen=lenact(prompt)

      RETURN
      END
