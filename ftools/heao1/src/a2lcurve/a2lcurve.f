**==a2lcue.spg  processed by SPAG 4.50F  at 13:51 on 19 Oct 1998
CH    PROGRAM a2lcurve             FROM PDP 11/70N RD1:[300,111]XRATE.FTN;1
CH                                 TO MICROVAX 3/26/88
CH
C    THIS PROGRAM WILL PROCESS FROM XRATES TAPES--
C    1.28S DSC, 5.12S DSC, AND 80MSEC MULTISC FOR
C    USE IN VARIOUS ANALYSIS PROGRAMS: FOLD,LIGHT
C    CURVE,ETC.  A DISC FILE IS THE OUTPUT.
CH
CH   7/16/88   E. ENG    MODIFIED CODE TO OUTPUT TO USER'S DISK
CH   11 Aug 1997  Jesse Allen  Reads ASCII data files instead of tapes
CH   12 Aug 1997   ""          Settings hardwired into code, scan data only
CH   15 Aug 1997   ""          Reads pointed or scanning from list
CH   11 Nov 1997   ""          Begin graunching FITS routines
CH   11 Dec 1997   ""          IMPLICIT NONE and use SLA subroutines.  Dead
CH                             weight code removed (e.g. HRAM checks)
CH   26 Feb 1998   ""          Reports on background calculations removed for
CH                             light curves with precalculated backgrounds
CH   19 Apr 1998   ""          Accepts multiple equinoxes and HMS DMS format
CH                              celestial coordinates, adds rootname to files,
CH                              correctly applies S/LFOV names for HED-1 & 2
CH   Jesse Allen (1.0.0 19 Apr 1998) Original working version
C
C
C
CH
CH   25 May 1998  L. Breedon  Changed input date and time formats to
CH                            dd/mm/yy and hh:m:ss.\
CH                            Now have input .lis file containing a list
CH                            of .raw files where the source was in FOV
CH                            in mission (.lis file is output from
CH                            tool A2SOURCE.f). This replaces code where
CH                            .raw filenames were constructed.\
CH                            A detector prompt is now only produced
CH                            if the input .lis file is for the HED detector\
CH                            Correct bug with respect to sla_PRECES routine\
CH  Lorraine Breedon (1.1.0 25 May 1998) Above modifications.
CH  Lorraine Breedon (1.2.1 26 Oct 1998) Remove COMMON blocks..pass variables
CH                                       via subroutine arguements\
CH                                       Modify code to only deal with
CH                                       1 detector at a time\
CH                                       Tidy up variable documentation\
CH                                       Simplify code into subroutines
CH                                       coords_work,
CH                                       open_1st_rawfile, set_fovflags,
CH                                       process_read_info, 
CH                                       qualchecks_and_ratecalcs, co_rawfile,
CH                                       lcclose_and_lcinfo \
CH                                       Tidy up code logic \
CH                                       Remove FORMAT statements \
CH                                       Improve output diagnostics \ 
CH
C *****************************************************************************
C
C SELECTOR TASK:
C   a2lcurve
C
C FILE:
C   a2lcurve.f
C
C DESCRIPTION:

C A2LCURVE generates a background-subtracted light curve from either the MED
C detector OR from one of the 3 HED detectors, for a single source position. 
C A lightcurve is generated from the summmed count-rates from up to four
C "discovery scalars" (a "discovery scalar" being a count-rate accumulated in
C various combinations of spectral and spatial windows - please see the A2 user
C guide for more information on discovery scalars).
C Regarding the background,  the user may select either (i) a backround
C generated from scans in the neighborhood of the source  OR (ii) a 
C background based on background models provided by Frank Marshall OR (iii)
C a fixed standard background rate (with errors) determined by the user.
C (ii) and (iii) are collectively referred to as 'MODEL' background. 
C
C PRIMARY VARIABLES: 
C
C In any arrays concerning the detectors, array element 1 is MED, and elements
C 2-4 are HED-1, 2, and 3 respectively.  Note that this is NOT the order used
C in the original XRATE code (which used 3 and 4 for HED-1 and 2, 5 for MED,
C and 6 for HED-3.  Elements 1 and 2 were placeholders for unused LED-1 and
C LED-2).
C
C
C  allflag         This is equivalent to the QFLAG (quality flag) column
C                  in a raw data file.
C  anglim          The start and stop scan angle limits for the output light curve.
C  avbak           The overall average background count rate ((bkgd+db)/fctim) for
C                  a given raw data file.
C  avb             Mean value of avbak for the total number of raw data files used 
C                  to construct the light curve (i.e. the overall average background
C                  count rate for the output light curve). 
C  avb2            The mean square of avbak. 
C  arrsz           Maximum number of raw data data files expected in the
C                  input listing file. 
C
C
C  backmode        Flag for the background mode (scanned versus model).
C  bkgd            If scanned background : mean background counts determined 
C                  from counter rows of a raw data file. If model background : either
C                  the level determined form Marshall tables or the level given
C                  by the user.
C  berr            Error in bkgd.
C  
C
C  chi             Goodness-of-fit between background points (counts) and mean
C                  overall background counts (for a given raw data file).
C  clnflag         Logical flag for clean data.
C  clobber         [NOT ENABLED]  Flag to indicate whether the program
C                  should overwrite existing light curves and/or log files
C                  or fail gracefully if files already exist.
C  counter         Indicates the number of rows in a raw data file, for which
C                  background was found.
C 
C 
C  dev             Difference between a background point and mean overall
C                  background counts for a given raw data file.
C  db              The mean value of dev.
C  diff            The difference between avb and the background rate (bkgd/fctim)... 
C                  this is calculated for the model background only. 
C  dtr             Conversion between degrees and radians.
C  detector        String name for the current detector.
C
C
C  eang            The angle from the spacecraft to the earth.
C  ecpra, ecpdec   B1950 FK4 R.A. and Dec. of the ecliptic pole.
C  ecp             3-point vector for the direction of the ecliptic pole
C  equinox, rastr, decstr, radeg, decdeg ........
C                  Variables for handling user provided source coordinates
C                  and equinoxes.
C  elcomflag       Logical flag for electron contamination.
C  emin, emax      The minimum and maximum energy range of the detector.
C
C
C  fctim           The integration time for the light curve (currently fixed
C                  at 5.12 s).
C
C
C  geoarea         The geometric area of the current detector.
C
C
C  hflag2          Flag array for a variety of information, such as electron
C                  contamination, all clean, earth occultation, etc.
C  hdsc            The counts in each 40.96 second record  for
C                  each of the eight scalars hdsc(*,*,[1-8]) and four detectors
C                  hdsc(*,*,*,[1-4]).  The array allows for six detectors due
C                  to original code heritage: the two LED detectors, however,
C                  were never used with a2lcurve or its predecessor code
C  hedflag         Flag indicating whether user requests HED data.
C  hdet            Detector requested (for which to generate light curve).
C  hds             Discovery scalar requests for the selected detector.
C  htmp            Temporary parameter for holding hds info.
C  hpt              ?
C
C
C  infmx           The total number of raw data files to be read from the 
C                  input listing file. 
C  iday, imsec     The day of 1977 and millisecond of day at the start of a
C                  40.96 s data record.
C  infile          The input listing file containg the list of raw data
C                  files to be read.
C  i, j, l         Variables used for looping.
C  idays, imsecs, idaye, imsece
C                  The start and end times in days of 1977 and milliseconds
C                  of day requested by the user. 
C  iday,imsec      Days of 1977 and milliseconds of that day (read from raw 
C                  data file). 
C
C  jf              Counter for all 4 detectors.
C  jtim            Array element number for xdays,xdaye (default=1).
C  jint            Counter to establish the number of times <xdays<xday<=xdaye
C                  for a given row in the raw data file (default=1).
C
C
C  lcunit          The I/O unit number assigned for the output light curve.
C  lfovflag        Flag indicating  the user's discovery scalar requests
C                  yield a large field-of-view.
C  logfile         Name of the log file for output from running the program.
C 
C  mcfilter        Flag for whether the user wants to filter data based on
C                  McIlwain L parameter.
C  mcilwainl       Current McIlwain L parameter value.
C  medflag         Flag indicating whether user requests MED data.
C  message         String to hold screen and log file messages.
C
C
C  ntim            Array element number for xdays,xdaye (default=1).
C  nb              Number of background points found.
C  nf              The current raw data file in the list.
C  nr              The current row in the raw data file.
C  nt              Counter to measure the number of times a raw data file is 
C                  to be read (however if using model background nt is fixed
C                  at 2). 
C  nrm             Indicates the number of rows (or records) in a raw
C                  data file, for which background was found. This is reset
C                  to zero after every 10 rows.
C  nfd             Number of raw data files for which background data will
C                  * probably * found. nfd incremented after some but not
C                  all data quality checks have been performed.
C  nbf             Number of raw data files for which background data HAS 
C                  been found. nbf incremented after all data quality checks
C                  have been performed.
C
C
C  parse, lchat, tchat, ichat, imessage ...............
C                  Variables associated with screen and/or log file
C                  recording of messages from the program.
C
C
C  qok             Logical flag array for clean data.
C  qcom            Logical flag array for electron contamination.
C  qang            Logical flag for whether the satellite scan angle is 
C                  within user input ANGlim range.
c  qbad            Logical data quality flag related to stability of 
C                  spacecraft spin-axis. If spin-axis stable qbad=.F.
C  qdir            Logical data quality flag related to A2 pointing direction. 
C                  If selected X-ray source is in A2 pointing direction qdir=.F.
C  qnb             Logical flag to indicate whether any background points have
C                  been found.
C  qf              Logical flag to indicate whether all rows in a given raw data
C                  file have been read (qf=.T.). If data file in the process of
C                  being read qf=.F.
C  qfird           Logical flag to indicate that either 
C                  background or source counts estimates are about to be
C                  determined for a given raw data file.
C  qcont           Logical flag to indicate that xday < xdaye.
C  qfir            ?
C  qel             Logical flag for *any* detector (either MED or any of the
C                  HED detectors) having electron contamination.
C  qan             Logical flag for an observation being `clean'
C  qr              Logical flag to indicate that background data has been found
C                  in a given row of a raw data file. 
C
C
C  rawunit         The I/O unit number assigned for the raw data file.
C  rawfile         Name of current raw data file .
C  rawfiles        Array containing the names of the raw data files.
C  ratefile        Name of output light curve file .
C  rang            Array of environment characteristics, such as the McIlwain
C                  L and B parameters.
C  row             Row number for currently read data in the raw data file.
C  rootname        Name of celestial X-ray source.
C  rmsd            Root mean square deviation of avbak for the total number of 
C                  raw data files found to have background data. 
C
C
C  spra, spdec     R.A. and Dec. (radians) of the spacecraft spin axis (Z axis).
C  sra, sdec       User provided source coordinates after transformation to
C                  B1950 FK4 celestial coordinates (radians).
C  sr, sd          Same as sra and sdec (degrees).
C  sor             3-point vector form of the source position.
C  sfovflag        Flag indicating  the user's discovery scalar requests
C                  yield a small field-of-view.
C  spinflag        Spin rate of the satellite is nominal.
C  status          Flag for the status of various I/O routines.  A non-zero
C                  value generally indicates a problem has occurred.
C
C 
C  userbackflag    Logical for whether the user has requested a scanned
C                  background or a model background (either Marshall or
C                  user estimate).
C
C
C  xdays, xdaye    User requested start and end time for an observation. 
C  xday            Current time in days of 1977 (including fraction of day)
C                  read from a given row in the raw data file.
C
C
C  yra, ydec       R.A. and Dec. (rad) of instrument pointing direction
C                  (Y axis).
C
C 
C
C
C
C TASK common block :
C   taskname       Common block used by FTOOLS to identify the tool running
C
C
C CALLED ROUTINES:
C  External library routines :
C
C      SLA routines        Starlink's Subroutine Library A (SLA) which
C                           perform a variety of coordinate system conversions.
C      parsera, parsedec   XANLIB routines to convert sexidecimal inputs into
C                           fractional degrees.
C      XWRITE, XAERROR     XANLIB routines for sending program output to
C      SETLOG               the terminal and/or log files with user set chatter
C                           levels.
C
C  Local routines :
C
C      getpar              Gets source parameters.
C      coords_work         Processes input coordinate information.
C      cvxyz               Convert a longitude and latitude into a 3-vector.
C      open_1st_rawfile    opens input file listing and places raw data
C                          filenames into an array. Sets medflag, hedflag
C                          logicals and opens the 1st raw data file. 
C      set_fovflags        Determines the FOV flags (both detector and 
C                          discovery scalar dependent). Allocates title
C                          to output light curve. 
C      readrawfits         Read the raw data file and return values for the 
C                          current data row.
C      process_read_info   Processes information from the 1st entire read of
C                          a raw data file.
C      co_rawfile          Closes a raw data file and then either re-opens it
C                          or opens the next raw data file in the list.
C      qualchecks_and_ratecalcs 
C                          Performs data quality checks and the background 
C                          and source rate calculations. Writes bsu'd source 
C                          count rate + error to output FITS lightcurve.
C      lcclose_and_lcinfo  Closes the output lightcurve file and issues
C                          the average background count rate for the lightcurve
C                          to the user.                       
C      marshall            If requested, calculate the current background
C                          based on the Frank Marshall background model.
C      xdirck              Check that the source is in the field of view
C                          and that the spin axis is stable (not wobbling).
C      dsc5                Read the 5.12 s discovery scalars and calculate
C                          the source count rate or background.
C      initlc, writelc, closelc
C                          Routines to initialize, write, and close the FITS
C                          light curve files.
C      effic               Determines the detector efficiency for a given set of
C                          discovery scalars.
C      scanan              Determines satellite scan angles given satellite
C                          spin RA,DEC and Y-axis RA,DEC.
C      scnchk              Determines whether satellite scan angle is within
C                          user requested range.
C      rotat,rotatx        Rotate satellite vectors about Z and X axes
C                          respectively.
C      dot                 Calculates the dot product of 2 vectors.
C      cross               Calculates the cross product of 2 vectors.

C ****************************************************************************
 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                        THE BASIC ALGORITHM

C If a MODEL background is selected :
C
C Each raw data file will be read only once (nt is fixed at 2). 
C PART A : Each row in a given raw data file will undergo various 'data quality'
C checks. For every row that passes the checks and the detector efficiency
C > 0.15 (i) the HDSc counts are read from the file (ii) the model background is
C subtracted from these 'source' counts (iii) the background subtracted 
C source count rate (+ error) is calculated and written to the output light
C curve file. 
C PART B : For every row that passes the checks, but the detector efficiency 
C < 2.5 x10(-6) (i) the HDSc counts are read from the file (ii) the running
C average deviation (+ve or -ve) between these 'bkgd' counts and the model
C background level is calculated.
C Finally, when the raw data file has been read completely, the sum of the 
C final average deviation and the model background is used to calculate 
C the average background rate for the file. This rate is issued to the screen.

C If a SCANNED background is selected :
C
C Each raw data file will be read twice (nt is intially set = 0). 
C PART A : When nt=1 the file will be read first to determine the scanned
C background.
C Each row in a given raw data file will undergo various 'data quality' checks. 
C For every row that passes the checks and the detector efficiency = 0
C the HDSc counts (i.e. scanned 'bkgd' counts) are read from the file. For 
C every such row a running average scanned background count is calculated. This
C running average is issued to the screen after every 10 rows of the raw data
C file where scanned background has been measured. When the file has been read
C completely the mean scanned background for the file has been determined.
C The file is then closed and re-opened again such that it can now be read to
C determine the source counts (now nt = 2). 
C PART B : Same as PART A for MODEL background algorithm  (except that the 
C background is the mean scanned background for the file rather than a model 
C background).
C PART C : For every row that passes the checks, but the detector efficiency 
C < 2.5 x10(-6) (i) the HDSc counts are read from the file (ii) the running
C average deviation (+ve or -ve) between these 'bkgd' counts and the mean 
C scanned background level is calculated. For every such row a running average 
C background rate is calculated (using the sum of the average deviation and 
C overall mean scanned background determined from the 1st read). This running
C average rate is issued to the screen after every 10 rows of the raw data 
C file for which 'bkgd' counts have been measured. 

C NOTE : A given row in a raw data file corresponds to a 40.96 s major frame
C and possesses 8 HDSc count values (a value for each of the eight 5.12sec
C records). Therefore when determining the background counts (detector eff=0) 
C for a given row, the maximum number of data records which may contribute
C to the background count is 8, and hence the maximum number of background 
C 'points' is 8. 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      SUBROUTINE A2LCUE
 
      IMPLICIT NONE
 
C Variables previously used in Common block declarations (now passed
C as arguments to subroutines). 
 
      INTEGER*2 HFLag2(4) , HDSc(5,8,8,6)
      INTEGER IDAy , IMSec 
      REAL EANg(2) , RANg(12) , SPRa(32) , SPDec(32) , YRA(32) , 
     &     YDEc(32)
c      LOGICAL QOK(4) , QCOm(4) , QANg
      REAL BER , DB
c      INTEGER JFD
      REAL ANGlim(2)

C common block 
      character(40) TASkname
      COMMON /TASK  / TASkname
 
C Local variables
 
      LOGICAL clnflag(4) , elcomflag(4) , userbackflag
      LOGICAL qcont , qfird , qnb , qf
c      LOGICAL qfir
      LOGICAL qdir , qbad , qr , medflag , hedflag
      LOGICAL mcfilter , clobber, goback,closefile
 
      INTEGER*2 allflag , spinflag
      INTEGER*2 hdet , hds(4) , hpt 
 
      INTEGER infmx, nprocess
      INTEGER nr , jint , nt , ntim , i , j ,l  , nb
      INTEGER nf , nbf , nrm  , nfd
      INTEGER idays , imsecs , idaye , imsece
c      INTEGER mft
      INTEGER jtim , rawunit , lcunit 
      INTEGER row , counter , backmode 
      INTEGER parse , lchat , tchat , ichat , status
       
      REAL emin , emax , fctim , avbak , chi 
      REAL bkgd , avb, avb2
      REAL xdays, xdaye, xday 
      REAL  mcilwainl, sr, sd, sra,sdec,ecp(3) , sor(3)
 
      DOUBLE PRECISION dtr,equinox
 
      INTEGER ARRSZ , LENACT
      PARAMETER (ARRSZ=200)
      character(16) detector , program , rootname
      character(40) ratefile , logfile
      character(80) imessage , rastr , decstr , infile
      character(160) rawfiles(ARRSZ) , message,rawfile
      

      INTEGER errstat 
 
c      DATA mft/320/
      DATA  dtr/.0174532925D0/
      DATA avb/0./ , avb2/0./ , qcont/.TRUE./
      DATA imessage , parse/' ' , 0/
      character(7) VERSION
      PARAMETER (VERSION='1.1.0')
 
C Begin program
      errstat = 0
      status = 0
      program = 'a2lcurve '
      TASkname = 'A2LCURVE '//VERSION
      CALL FTCMSG
 
C Initialise Variables used for processing 
 
       nr = 1
       jint = 0
       counter = 0
       nt = 0
       fctim = 5.12
       goback=.FALSE.
       closefile=.FALSE.
c       qfir = .TRUE.
       qfird = .TRUE.
       qnb = .FALSE.
       qf = .FALSE.
       nbf = 0
c       JFD = 0
       ntim = 1
       xdays=0.0
       xdaye=0.0
       xday=0.0
       idays=0
       imsecs=0
       idaye=0
       imsece=0
       nb=0
       bkgd=0.0
       row = 0

C Get parameters and set terminal and log chat levels

  
      CALL GETPAR(infile,rootname,rastr,decstr,equinox,idays,
     &            imsecs,idaye,imsece,hdet,hds,backmode,bkgd,BER,
     &            anglim,
     &            mcfilter,mcilwainl,lchat,tchat,clobber,status)
      IF ( status.NE.0 ) THEN
         message = ' Failure in attempting to retrieve input parameters'
         CALL XAERROR(message,1)
         GOTO 99999
      ENDIF



c open the log file if necessary tchat>=lchat
c reset the internal chatness to lchat
 
      CALL XCHATY(tchat,lchat)
      ichat = lchat

C set the flag to indicate whether a model (MARSHALL or user input)
C background has been selected instead of the scanned background.
C Userbackflag=.F. for scanned background ; .T. for model background.

      userbackflag = .FALSE.
      IF ( backmode.NE.0 ) userbackflag = .TRUE.


C Convert Right Ascension and Declination inputs into B1950.0 coordinates
C in degrees etc. 

      status=0
      CALL COORDS_WORK(rastr,decstr,dtr,equinox,ecp,sra,sdec,
     &                sor,status) 
      IF ( status.NE.0 ) THEN
         message = ' Failure during coord transformation calculations'
         CALL XAERROR(message,1)
         GOTO 99999
      ENDIF

      sr = sra/dtr
      sd = sdec/dtr

 
C Open input file listing and place rawdata filenames in memory. Also
C set medflag, hedflag logicals. Finally open the 1st raw datafile

      status=0
      CALL OPEN_1ST_RAWFILE(infile, hdet,hds,medflag,hedflag,nf,
     &               ARRSZ,infmx,rawunit,rawfiles,
     &               rawfile,ichat,status)
      IF ( status.NE.0 ) THEN
         message = ' Failure obtaining or opening raw data file'
         CALL XAERROR(message,1)
         GOTO 99999
      ENDIF



C Determine the FOV flags (detector and discovery scalars dependent).
C Allocate a title to the output ratefile (lightcurve). 

 
      CALL SET_FOVFLAGS(rootname,ratefile,hdet,hds,emin,emax,
     &                   detector)

    
C Determine the observation start and stop days requested by the 
C user.

       
      xdays = FLOAT(idays) + FLOAT(imsecs)/8.64D+07
      xdaye = FLOAT(idaye) + FLOAT(imsece)/8.64D+07



C Give useful information to terminal and log file

      i = INDEX(TASkname,' ')
      message = TASkname(:LENACT(TASkname))//' start light curves'
      CALL XWRITE(message,ichat)
      logfile = '+'//TASkname(1:i-1)//'.log'
      IF ( lchat.GE.tchat ) CALL SETLOG(imessage,parse,logfile,' ')
      message = ' '
      CALL XWRITE(message,ichat)
      i = INDEX(rootname,' ')
      WRITE (message,'('' OBJECT NAME '',A16)') rootname(1:i-1)
      CALL XWRITE(message,ichat)
      WRITE (message,'('' SOURCE RA AND DEC (1950) '',2F8.3)') sr , sd
      CALL XWRITE(message,ichat)
      WRITE (message,'('' DETECTOR '',I4,'' DISCOVERY SCALARS '',4I3)')
     & hdet,(hds(j),j=1,4)
      CALL XWRITE(message,ichat)
      WRITE (message,'( '' SCAN ANGLE LIMITS '',2F10.3)') 
     & (ANGlim(l),l=1,2)
      CALL XWRITE(message,ichat)
      WRITE (message,'( '' BACKGROUND TYPE '',I2)') backmode
      CALL XWRITE(message,ichat)

C Create header for output lightcurve

      status=0
      CALL FTGIOU(lcunit,status)
      CALL INITLC(lcunit,ratefile,rootname,sr,sd,detector,emin,
     &               emax,userbackflag,clobber,status)
      IF ( status.NE.0 ) THEN
            WRITE (message,'('' Error creating '', A40)') ratefile
            CALL XWRITE(message,ichat)
            CALL FTGMSG(message)
            CALL XWRITE(message,ichat)
            WRITE (message,'('' FITSIO status = '', I3)') status
            CALL XWRITE(message,ichat)
            status = 0
            CALL FTCLOS(lcunit,status)
            CALL FTFIOU(lcunit,status)
            GOTO 99999
      ENDIF
 

C Here goes the main processing >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C Process information from the reads of a raw data file.

 700   CALL PROCESS_READ_INFO(userbackflag,rawfile,goback,nt,qf,
     &                        nb,nbf,bkgd,db,closefile,avbak,
     &                        chi,qnb,hdet,qfird,avb,avb2,qcont,
     &                        fctim,nfd,nf,ichat)

       IF (closefile) goto 1700
 
 1300 DO WHILE ( .TRUE. )

C  If using model background, inform user that a raw data file is
C about to be read to determine the source and bkgd counts. 
         IF (userbackflag .AND. nr.EQ.1) THEN
            message = ' '
            CALL XWRITE(message,ichat)
            WRITE (message,'(''# Processing RAW DATA FILE '',I5)') nf
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' >>>>>> '',a100)') rawfile
            CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)

            WRITE (message,
     &'('' Starting source & bkgd count calcs...'')')
            CALL XWRITE(message,ichat)
         ENDIF  

C Read the raw data files.  Status will be returned as -99 if the end of the
C raw data file is reached

         status=0
         CALL READRAWFITS(rawunit,medflag,hedflag,nr,IDAy,IMSec,
     &                    elcomflag,allflag,clnflag,spinflag,EANg,RANg,
     &                    SPRa,SPDec,YRA,YDEc,HDSc,status)

         IF ( status.EQ.-99 ) THEN
            status = 0
            GOTO 1400
         ELSEIF ( status.LT.0 ) THEN
            GOTO 99999
         ELSEIF ( status.NE.0 ) THEN
            WRITE (message,'('' Error reading data files at row '', i6)'
     &             ) nr
            CALL XAERROR(message,1)
            CALL FTGMSG(message)
            CALL XAERROR(message,1)
            WRITE (message,'('' FITSIO status = '', I3)') status
            CALL XAERROR(message,1)
            GOTO 99999
         ENDIF
         nr = nr + 1


 
        
c         qfir = .FALSE.
C This is for scanned background only :
         IF (.NOT. userbackflag .AND. qf .AND. nt.EQ.2 ) THEN
           message = ' '
           CALL XWRITE(message,ichat)
           WRITE (message,'('' Processing RAW DATA FILE '',I5)') nf
           CALL XWRITE(message,ichat)
           IF ( .NOT.qfird ) THEN
            WRITE (message,'('' Starting source count calcs ... '')')
            CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)
           ENDIF
           IF ( qnb ) THEN
            WRITE (message,'('' Calculations failed...file BAD'')')
            CALL XWRITE(message,ichat)
           ENDIF
 
         ENDIF
 
 
         qf = .FALSE.
 
   
         IF ( .NOT.(.NOT.userbackflag .AND. nt.EQ.2 .AND. qnb) ) THEN
            IF ( nt.EQ.0 ) nt = 1
            xday = FLOAT(IDAy) + FLOAT(IMSec)/8.64D+07
            
C MOD 15 Aug 1997  Use scanned background values if requested
C Set the background according to the observation time
C (See the HEAO A-2 User's Guide for details)
 
            
            IF ( (userbackflag) .AND. (backmode.EQ.1) )
     &           CALL MARSHALL(hdet,hds,xday,fctim,bkgd,BER)
 
             
            jtim=1
            IF ( xdays.LT.xday .AND. xday.LE.xdaye ) jint = jtim

            IF ( xdays.LT.xday .AND. xday.LT.xdaye ) GOTO 1600
              qcont = .FALSE.
            IF ( xdaye.GT.xday ) qcont = .TRUE.
            IF ( jint.LT.ntim ) qcont = .TRUE.
            IF ( .NOT.(qcont) ) THEN
               IF ( .NOT.(.NOT.qcont .AND. nt.EQ.1) ) THEN
                  nfd = nfd + 1
                   
 
                  IF ( userbackflag ) then
                      goback = .TRUE.
                      message = ' '
                      CALL XWRITE(message,ichat)
                      WRITE (message,
     &'(''WARNING : Time in current row > obsn end time'')')
                      CALL XWRITE(message,ichat)
                      WRITE (message,
     &'(''WARNING : Will not process any more raw files after this'')')
                      CALL XWRITE(message,ichat)

  
                       GOTO 700
                  ENDIF
                  message = ' '
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'(''WARNING : Time in current row > obsn end time'')')
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'(''WARNING : Will not process any more raw files after this'')')
                  CALL XWRITE(message,ichat)



                  GOTO 1700
 
               ENDIF
  
               GOTO 1400
            ENDIF
         ENDIF
      ENDDO


 1400 IF ( qf ) GOTO 1700
   
      counter = 0
      qf = .TRUE.
      IF ( nt.EQ.2 ) nf = nf + 1
      IF ( nt.EQ.2 .AND. .NOT.qfird ) nfd = nfd + 1
      nt = MOD(nt+1,3)


C Close the current raw data file and then either re-open it OR
C open the NEXT file in the list (see the comments in 
C CO_RAWFILE.f for further explanation).

       status=0
       CALL CO_RAWFILE(rawunit,nf,infmx,nr,ARRSZ,rawfiles,rawfile,
     &                      closefile,ichat,status)
       IF ( status.NE.0 ) THEN
         message = ' Failure closing or opening raw data file in list'
         CALL XAERROR(message,1)
         GOTO 99999
       ENDIF

      IF (closefile) GOTO 1900

      GOTO 700
 
C Reject data with McIlwain L greater than allowed.  The recommended maximum
C value for good data is 1.2 Earth radii

       
 1600 IF ( .NOT.((mcfilter) .AND. (RANg(5).GT.mcilwainl)) ) THEN
  
C Perform all remaining data quality checks. 
C If OK go ahead and perform background and source rate calculations.
C Then write bsu'd source rate to the output light curve.

          status=0
          CALL QUALCHECKS_AND_RATECALCS(userbackflag,clnflag,
     &        allflag,elcomflag,spinflag,sor,ecp,qdir,qbad,hpt,
     &        qfird,nf,nfd,lcunit,nt,hdet,hds,bkgd,nb,ber,db,chi,
     &                      IDAy,IMSec,HFLag2,ANGlim,
     &                      SPRa,SPDec,rawfile,
     &                      YRA,YDEc,HDSc,sra,sdec,row,
     &                      qr,counter,nrm,avbak,fctim,ichat,status)
         IF ( status.NE.0 ) THEN
            message = 'Failure in quality checking or rate calculations'
            CALL XAERROR(message,1)
            GOTO 99999
         ENDIF
      ENDIF
 
      GOTO 1300

C Close the raw data file
 
1700    CALL FTCLOS(rawunit,status)
        CALL FTFIOU(rawunit,status)

      IF ( status.NE.0 ) THEN
         message = 'Error closing raw data file'
         CALL XAERROR(message,1)
         CALL FTGMSG(message)
         CALL XAERROR(message,1)
         WRITE (message,'('' FITSIO status = '', I3)') status
         CALL XAERROR(message,1)
         GOTO 99999
      ENDIF
 
1900  continue 
         IF (userbackflag .and. (nf.lt.infmx .or. .NOT.(qcont)))
     & goto 2000

C Don't forget to issue info about the final raw data file 

         IF ( nb.NE.0 ) THEN
            nbf = nbf+ 1
            avbak = (bkgd+DB)/fctim
            message = ' '
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' Overall avrage bkgd for raw data file '',F10.3,'' cnts/s'')') 
     & avbak 
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' This is file number '',I3,'' for which bkgd was measured'')')
     & nbf
            CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)
            avb = (avb*(nbf-1)+avbak)/(nbf)
            avb2 = (avb2*(nbf-1)+avbak**2)/(nbf)            
         ELSE
            IF (userbackflag) THEN
             message = ' '
             CALL XWRITE(message,ichat)
             message = ' WARNING: File BAD. Data failed quality checks.'
             CALL XWRITE(message,ichat)
             message = ' (For reasons please see a2lcurve.hlp file)'
             CALL XWRITE(message,ichat)
             message = ' Unable to determine source cnts since file BAD'
             CALL XWRITE(message,ichat)
            ENDIF
         ENDIF


2000     continue

        If (nf .gt. infmx) then
           nprocess = nf - 1
        ELSE
           nprocess = nf
        ENDIF
        message = ' '
        CALL XWRITE(message,ichat)
        WRITE (message,
     &'(''## Processed '',I3,'' RAW FILES out of a total of '',I3)')
     & nprocess,infmx
        CALL XWRITE(message,ichat)


 
C Close the output lightcurve file and issue
C the average background count rate for the lightcurve to the user.

        status=0

        CALL LCCLOSE_AND_LCINFO(userbackflag,avb,avb2,bkgd,fctim,
     &                        nbf,nf,infmx,lcunit,row,hdet,
     &                        hds,ratefile,ichat,status)
        IF ( status.NE.0 ) THEN
            message = 'Failure closing lightcurve '
            CALL XAERROR(message,1)
            GOTO 99999
        ENDIF
        message = ' '
        CALL XWRITE(message,ichat)



99999 END

